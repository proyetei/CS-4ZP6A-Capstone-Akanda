-- | @shake@ build rules for @panbench@ modules.
module Panbench.Shake.Lang
  ( -- $shakeLang
    findDefaultExecutable
  , cleanBuildArtifacts
    -- $shakeGenerate
  , GenerateModule(..)
  , needModule
  , needModules
  , generatorRules
  ) where

import Development.Shake
import Development.Shake.Classes

import GHC.Generics

import Numeric.Natural

import System.Directory (findExecutable)
import System.FilePath

import Panbench.Shake.File
import Panbench

import Panbench.Lang qualified as Lang

-- * Shake rules for language-dependent things
--
-- $shakeLang

-- | Find the default executable for a given @'Lang'@.
--
-- Always returns an absolute path.
findDefaultExecutable :: Lang -> Action FilePath
findDefaultExecutable lang =
  liftIO (findExecutable (Lang.defaultExecutable lang)) >>= \case
    Just bin -> pure bin
    Nothing ->
      fail $ unlines $
      [ "Could not find executable for " <> show lang <> " in the path."
      , "Perhaps it is not installed?"
      ]

-- | Remove all build artifacts for a @'Lang'@ in a directory.
cleanBuildArtifacts :: Lang -> FilePath -> Action ()
cleanBuildArtifacts lang dir =
  removeFilesAfter dir (Lang.buildArtifacts lang)

-- * Shake rules for generating per-language modules
--
-- $shakeGenerate
--
-- We will need to write out module files to disk to be able
-- to pass them off to our various proof assistants. Moreover,
-- we also need to write multiple versions of the same file with
-- different size parameters. We will use the following convention
-- for paths:
--
-- > _build/lang/gen/n/mod.ext
--
-- Where @lang@ is the language name, @gen@ is the name of the generator,
-- @n@ is the size parameter, and @mod.ext@ is the rendered module file.
--
-- WARNING: This convention is subject to change, and should not be relied
-- on by external programs.

-- | Query for rendering a module using a generator.
data GenerateModule = GenerateModule
  { generatorName :: String
  -- ^ The generator to use.
  , generatorLang :: Lang
  -- ^ The language to generate.
  , generatorSize :: Natural
  -- ^ Size of the module to generate.
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult GenerateModule = ()

-- | Construct a relative @'FilePath'@ for a file
-- that will contain the result of a @'GenerateModule'@ query.
generatorOutputDir :: GenerateModule -> FilePath
generatorOutputDir GenerateModule{..} =
  "_build" </> Lang.name generatorLang </> generatorName </> show generatorSize
  </> generatorName <.> Lang.fileExt generatorLang

needModule :: GenerateModule -> Action (FilePath, FilePath)
needModule gen = do
  (path, _) <- askFileCacheOracle gen
  return (splitFileName path)

needModules :: [GenerateModule] -> Action [(FilePath, FilePath)]
needModules gens = do
  paths <- fmap fst <$> asksFileCacheOracle gens
  return (fmap splitFileName paths)

-- | Rules for module generation.
generatorRules :: Rules ()
generatorRules = do
  addFileCacheOracle generatorOutputDir (\_ -> pure ()) \GenerateModule{..} -> do
    need ["generators" </> "app" </> generatorName <.> "hs"]
    Stdout out <- command [] "cabal" ["run", generatorName, "--", "--size", show generatorSize, "--language", Lang.name generatorLang]
    pure ((), out)
