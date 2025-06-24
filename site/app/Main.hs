{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Data.List
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Text.Read (readMaybe)
import Numeric.Natural
import GHC.Generics

import Development.Shake
import Development.Shake.Classes (Hashable, Binary, NFData)

import System.FilePath

import Panbench.Internal

import Print.Agda qualified as Agda
import Print.Idris qualified as Idris
import Print.Lean qualified as Lean
import Print.Rocq qualified as Rocq

import Grammar
import Tests

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
parseModuleBaseName :: Lang -> FilePath -> Action String
parseModuleBaseName lang path =
  let (noExt, ext) = splitExtension path in
  if ext == langExt lang then
    pure $ takeFileName noExt
  else
    fail $ unlines $
    [ "Expected path '" <> (path -<.> langExt lang) <> "' for " <> show lang
    , "but got" <> path
    ]

-- | Parse a module path into a language, size, and base name.
parseModulePath :: FilePath -> Action (Lang, Natural, String)
parseModulePath path = do
  lang <- parseModulePathLang path
  n <- parseModulePathSize path
  base <- parseModuleBaseName lang path
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

main :: IO ()
main = shakeArgs (shakeOptions {shakeFiles="_build"}) do
  moduleRules
