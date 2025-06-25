{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad

import Data.Functor
import Data.List
import Data.Map.Strict (Map)
import Data.String
import Data.Set (Set)
import Data.Text (Text)
import Data.Traversable

import Data.Aeson qualified as JSON
import Data.Aeson.Encoding qualified as JSON
import Data.ByteString.Lazy qualified as LBS
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.IO qualified as T
import Data.Text.Lazy.Encoding qualified as LT


import Text.Read (readMaybe)
import Numeric.Natural
import GHC.Generics

import Development.Shake
import Development.Shake.Classes (Hashable, Binary, NFData)

import Graphics.Vega.VegaLite (VegaLite)
import Graphics.Vega.VegaLite qualified as VL

import System.Directory qualified as Dir
import System.Directory (findExecutable)
import System.FilePath
import System.IO.Error
import System.IO

import Text.Blaze.Html.Renderer.Utf8 qualified as H

import Panbench.Internal
import Panbench.HTML

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
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (Hashable, Binary, NFData)

instance JSON.ToJSON Lang where
  toJSON Agda = JSON.String "agda"
  toJSON Idris = JSON.String "idris"
  toJSON Lean = JSON.String "lean"
  toJSON Rocq = JSON.String "rocq"

-- [FIXME: Reed M, 24/06/2025] This is all kinda bad.
-- Should think about a general mechanism for what a language is.

-- | Get the name of a language.
langName :: Lang -> String
langName Agda = "agda"
langName Idris = "idris"
langName Lean = "lean"
langName Rocq = "rocq"

-- | Get the extension type for a given @Lang@.
langExt :: Lang -> String
langExt Agda = ".agda"
langExt Idris = ".idr"
langExt Lean = ".lean"
langExt Rocq = ".v"

langFlags :: Lang -> FilePath -> [String]
langFlags Agda file = ["+RTS", "-M3.0G", "-RTS", file]
langFlags Idris file = ["--check", file]
langFlags Lean file = ["-D", "maxRecDepth=2000", "-D", "maxHeartbeats=0", file]
langFlags Rocq file = [file]

-- | Get the name of the binary for a language.
langBinName :: Lang -> String
langBinName Agda = "agda"
langBinName Idris = "idris2"
langBinName Lean = "lean"
langBinName Rocq = "coqc"

-- | Lowercase string representations of each language name.
--
-- Used for determining the language from a path.
langNames :: Map String Lang
langNames =
  Map.fromList $ [minBound..maxBound] <&> \lang ->
    (langName lang, lang)

-- | Rule for finding a language's binary.
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

-- | Query for benchmarking a module against a single size and language.
data BenchmarkLang = BenchmarkLang
  { benchLang :: Lang
  , benchSize :: Natural
  , benchModuleName :: String
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult BenchmarkLang = BenchmarkExecStats

benchmarkLangOracle
  :: (BenchmarkExec -> Action BenchmarkExecStats)
  -> BenchmarkLang -> Action BenchmarkExecStats
benchmarkLangOracle benchmark BenchmarkLang{..} = do
  let dir = "_build" </> langName benchLang </> show benchSize
  let file = benchModuleName -<.> langExt benchLang
  need [dir </> file]
  bin <- findLangBin benchLang
  benchmark (BenchmarkExec bin (langFlags benchLang file) [] dir)

-- | Benchmarking matrix query.
data BenchmarkMatrix = BenchmarkMatrix
  { benchMatrixLangs :: Set Lang
  -- ^ What languages should we include in the matrix?
  , benchMatrixSamples :: [Natural]
  -- ^ Parameters to sample at.
  , benchMatrixModuleName :: String
  -- ^ Module to benchmark.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, Binary, NFData)


newtype BenchmarkMatrixResult = BenchmarkMatrixResult (Map Lang [(Natural, BenchmarkExecStats)])
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable, Binary, NFData)

-- | We need this somewhat annoying instance to make our data a bit more @vega-lite@
-- friendly.
instance JSON.ToJSON BenchmarkMatrixResult where
  toJSON (BenchmarkMatrixResult results) =
    JSON.toJSON $ concat $ Map.toList results <&> \(lang, stats) ->
      stats <&> \(n, BenchmarkExecStats{..}) ->
        JSON.object
        [ ("lang", JSON.toJSON lang)
        , ("size", JSON.toJSON n)
        , ("user", JSON.toJSON benchUserTime)
        , ("system", JSON.toJSON benchSystemTime)
        , ("rss", JSON.toJSON benchMaxRss)
        ]

type instance RuleResult BenchmarkMatrix = BenchmarkMatrixResult

benchmarkMatrixOracle
  :: (BenchmarkLang -> Action BenchmarkExecStats)
  -> BenchmarkMatrix -> Action BenchmarkMatrixResult
benchmarkMatrixOracle benchmark BenchmarkMatrix{..} =
  fmap BenchmarkMatrixResult $
  sequence $
  flip Map.fromSet benchMatrixLangs \lang ->
  for benchMatrixSamples \n -> do
    stats <- benchmark (BenchmarkLang lang n benchMatrixModuleName)
    pure (n, stats)

-- * Report rendering

main :: IO ()
main = shakeArgs (shakeOptions {shakeFiles="_build"}) do
  benchmarkExec <- addOracle benchmarkExecOracle
  benchmarkLang <- addOracle (benchmarkLangOracle benchmarkExec)
  -- Why isn't this caching?
  benchmarkMatrix <- addOracleCache (benchmarkMatrixOracle benchmarkLang)
  moduleRules

  phony "clean" do
    removeFilesAfter "_build" ["agda/*", "lean/*", "idris/*", "rocq/*", "*.html"]

  "_build/index.html" %> \out -> do
    need [vegaSrc, vegaLiteSrc, vegaEmbedSrc]
    vegaJs <- liftIO $ T.readFile vegaSrc
    vegaLiteJs <- liftIO $ T.readFile vegaLiteSrc
    vegaEmbedJs <- liftIO $ T.readFile vegaEmbedSrc
    stats <- benchmarkMatrix (BenchmarkMatrix (Set.fromList [Agda, Idris, Lean, Rocq]) [2^n | n <- [(0 :: Int)..10]] "LetExample")
    writeBinaryFileChanged out
      $ H.renderHtml
      $ benchmarkHtml vegaJs vegaLiteJs vegaEmbedJs
      $ JSON.toJSON stats
    where
      vegaSrc = "web/vega@5.10.js"
      vegaLiteSrc = "web/vega-lite@4.7.0.js"
      vegaEmbedSrc = "web/vega-embed@6.3.2.js"
