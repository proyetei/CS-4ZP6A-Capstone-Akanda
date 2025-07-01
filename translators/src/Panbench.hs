{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
-- | Panbench utilities.
module Panbench
  ( -- $generators
    panbenchMain
  -- * Re-exports
  , Lang(..)
  , module Grammar
  ) where

import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

import Numeric.Natural

import Options.Applicative

import Panbench.Lang (Lang)
import Panbench.Lang qualified as Lang
import Grammar

-- * Generators
--
-- $generators

-- | Options to pass to a generator.
data PanbenchCmd
  = GenerateCmd Lang Natural
  | BaselineCmd Lang
  deriving (Show)

langOptParser :: Parser Lang
langOptParser =
  option @Lang auto $ mconcat
  [ long "language"
  , metavar "LANG"
  , help "The language to generate for."
  , completeWith (Lang.name <$> Set.toList Lang.allLangs)
  ]

sizeOptParser :: Parser Natural
sizeOptParser =
  option @Natural auto $ mconcat
  [ long "size"
  , metavar "SIZE"
  , help "The size of the module to generate."
  ]

generateCmdParser :: Parser PanbenchCmd
generateCmdParser = GenerateCmd <$> langOptParser <*> sizeOptParser

baselineCmdParser :: Parser PanbenchCmd
baselineCmdParser = BaselineCmd <$> langOptParser

panbenchCmdParse :: Parser PanbenchCmd
panbenchCmdParse =
  subparser $ mconcat
    [ command "generate" $ info generateCmdParser (progDesc "Generate a module of a given size.")
    , command "baseline" $ info baselineCmdParser (progDesc "Generate a \"baseline\" module containing just imports.")
    ]


-- | Construct a @panbench@ generator binary.
--
-- This function is intended to be used as the @main@ function
-- of a @panbench@ generator, and will read in a size and language
-- as @--size@ and @--language@ arguments.
panbenchMain
  :: Text
  -- ^ The name of the generator.
  -> [Import]
  -- ^ Imports.
  -> (Natural -> [Definition])
  -- ^ Generate a module of a given size.
  -> IO ()
panbenchMain name imports gen = do
  cmd <- customExecParser (prefs showHelpOnEmpty) $
    info (panbenchCmdParse <**> helper) $ mconcat
      [ fullDesc
      , header ("panbench generator for " <> T.unpack name)
      ]
  case cmd of
    GenerateCmd lang size ->
      T.putStr (Lang.render lang $ Module name imports (gen size))
    BaselineCmd lang ->
      T.putStr (Lang.render lang $ Module name imports [])
