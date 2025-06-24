{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad

import Data.Aeson
import Data.List
import Data.Map.Strict (Map)

import Data.ByteString.Lazy qualified as LBS
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map

import Text.Read (readMaybe)
import Numeric.Natural
import GHC.Generics

import Development.Shake
import Development.Shake.Classes (Hashable, Binary, NFData)

import System.Directory qualified as Dir
import System.Directory (findExecutable)
import System.FilePath
import System.IO.Error
import System.IO

import Panbench.Internal

import Print.Agda qualified as Agda
import Print.Idris qualified as Idris
import Print.Lean qualified as Lean
import Print.Rocq qualified as Rocq

import Grammar
import Tests

-- * Shake Miscellania
--
-- The following code was adapted from @writeFileChanged@ in @General.Extras@ in @shake-0.19.8@.
-- We need to be able to write binary files, which shake does not support OOTB.

createDirectoryRecursive :: FilePath -> IO ()
createDirectoryRecursive dir = do
    x <- try @IOException $ Dir.doesDirectoryExist dir
    when (x /= Right True) $ Dir.createDirectoryIfMissing True dir

removeFile_ :: FilePath -> IO ()
removeFile_ x =
    Dir.removeFile x `catch` \e ->
        when (isPermissionError e) $ handle @IOException (\_ -> pure ()) $ do
            perms <- Dir.getPermissions x
            Dir.setPermissions x perms{Dir.readable = True, Dir.searchable = True, Dir.writable = True}
            Dir.removeFile x

writeBinaryFileChanged :: (MonadIO m) => FilePath -> LBS.ByteString -> m ()
writeBinaryFileChanged name x = liftIO $ do
    createDirectoryRecursive $ takeDirectory name
    exists <- Dir.doesFileExist name
    if not exists then LBS.writeFile name x else do
        changed <- withFile name ReadMode $ \h -> do
            src <- LBS.hGetContents h
            pure $! src /= x
        when changed $ do
            removeFile_ name -- symlink safety
            LBS.writeFile name x

-- | The current tests that we have, keyed by their name.
modules :: Map String (Natural -> Module)
modules = Map.fromList
  [ ("LetExample", tests IntMap.! 1) -- [FIXME: Reed M, 23/06/2025] Stupid hack just to test for now.
  ]

-- * Rendering
--
-- We will need to write out module files to disk to be able
-- to pass them off to our various proof assistants. Moreover,
-- we also need to write multiple versions of the same file with
-- different size parameters. We will use the following convention
-- for paths:
--
-- > _build/lang/n/mod.ext
--
-- Where @lang@ is the language name, @n@ is the size parameter, and @mod.ext@
-- is the rendered module file.
--
-- We could also choose to bunch up all the rendered files in the same directory,
-- but this makes things a bit cluttered.

-- | Supported languages.
data Lang = Agda | Idris | Lean | Rocq
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, Binary, NFData)

-- | Lowercase string representations of each language name.
--
-- Used for determining the language from a path.
langNames :: Map String Lang
langNames = Map.fromList
  [ ("agda", Agda)
  , ("idris", Idris)
  , ("lean", Lean)
  , ("rocq", Rocq)
  ]

-- | Get the extension type for a given @Lang@.
langExt :: Lang -> String
langExt Agda = ".agda"
langExt Idris = ".idr"
langExt Lean = ".lean"
langExt Rocq = ".v"

langBinName :: Lang -> String
langBinName Agda = "agda"
langBinName Idris = "idris2"
langBinName Lean = "lean"
langBinName Rocq = "coqc"

findLangBin :: Lang -> Action String
findLangBin lang =
  liftIO (findExecutable (langBinName lang)) >>= \case
    Just bin -> pure bin
    Nothing ->
      fail $ unlines $
      [ "Could not find executable for " <> show lang <> " in the path."
      , "Perhaps it is not installed?"
      ]

-- | Print a module for a given @Lang@.
printLangModule :: Lang -> Module -> String
printLangModule Agda = Agda.printModule
printLangModule Idris = Idris.printModule
printLangModule Lean = Lean.printModule
printLangModule Rocq = Rocq.printModule

-- | Get a the language from a module path.
parseModulePathLang :: FilePath -> Action Lang
parseModulePathLang path =
  case Map.lookup (takeFileName $ takeDirectory $ takeDirectory path) langNames of
    Just lang -> pure lang
    Nothing -> fail $ unlines $
      [ "Could not determine language from path '" <> path <> "'"
      , "By convention, paths should take the form"
      , "  '_build/lang/n/mod.ext'"
      , "where 'lang' is language. "
      , "Currently supported languages are " <> intercalate ", " (Map.keys langNames) <> "."
      ]

-- | Get the size from a module path.
parseModulePathSize :: FilePath -> Action Natural
parseModulePathSize path =
  case readMaybe $ takeFileName $ takeDirectory path of
    Just n -> pure n
    Nothing -> fail $ unlines $
      [ "Could not determine size from path '" <> path <> "'"
      , "By convention, paths should take the form"
      , "  '_build/lang/n/mod.ext'"
      , "where 'n' is the size of the test."
      ]

-- | Get the base name from a module path, and verify that
-- the file extension is appropriate for a given @Lang@.
parseModuleBaseName :: String -> FilePath -> Action String
parseModuleBaseName expectedExt path =
  let (noExt, actualExt) = splitExtension path in
  if actualExt == expectedExt then
    pure $ takeFileName noExt
  else
    fail $ unlines $
    [ "Expected path '" <> (path -<.> expectedExt)
    , "but got" <> path
    ]

-- | Parse a module path into a language, size, and base name.
parseModulePath :: FilePath -> Action (Lang, Natural, String)
parseModulePath path = do
  lang <- parseModulePathLang path
  n <- parseModulePathSize path
  base <- parseModuleBaseName (langExt lang) path
  pure (lang, n, base)

-- | Compilation rule for a module.
moduleRules :: Rules ()
moduleRules =
  "_build/*/*/*" %> \out -> do
    (lang, n, base) <- parseModulePath out
    case Map.lookup base modules of
      Just module_ ->
        writeFileChanged out (printLangModule lang $ module_ n)
      Nothing ->
        putError $ unlines $
        [ "No benchmark available for " <> base
        , "Current benchmarks are:"
        ] ++ Map.keys modules

-- * Language benchmarking
--
-- We will use the following convention for benchmarking results.
--
-- > _build/lang/n/bench/mod.json

-- | Rules for producing benchmarking results relative to a benchmark oracle.
langBenchmarkRules :: (Benchmark -> Action BenchmarkStats) -> Rules ()
langBenchmarkRules benchmark =
  "_build/*/*/bench/*.json" %> \out -> do
    -- Nasty directory munging. Could probably be more elegant,
    -- but doesn't really matter.
    lang <- parseModulePathLang (takeDirectory out)
    let dir = takeDirectory $ takeDirectory out
    let file = takeBaseName out -<.> langExt lang
    need [dir </> file]
    -- [FIXME: Reed M, 23/06/2025] Pull this out into an oracle.
    bin <- findLangBin lang
    -- [FIXME: Reed M, 23/06/2025] Language options.
    stats <- benchmark (Benchmark bin [file] [] dir)
    writeBinaryFileChanged out $ encode stats

main :: IO ()
main = shakeArgs (shakeOptions {shakeFiles="_build"}) do
  benchmark <- addBenchmarkOracle
  langBenchmarkRules benchmark
  moduleRules
