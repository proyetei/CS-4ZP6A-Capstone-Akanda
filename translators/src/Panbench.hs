-- | Panbench utilities.
module Panbench
  ( -- $languages
    Lang(..)
  , renderLang
    -- $generators
  , panbenchMain
  ) where

import Numeric.Natural
import Text.ParserCombinators.ReadP qualified as P
import Text.Read

import Options.Applicative

import Print.Agda qualified as Agda
import Print.Idris qualified as Idris
import Print.Lean qualified as Lean
import Print.Rocq qualified as Rocq
import Grammar

-- * Languages
--
-- $languages

-- | Possible languages to generate.
data Lang = Agda | Idris | Lean | Rocq
  deriving (Eq, Ord, Show)

instance Read Lang where
  readPrec = lift $ P.choice
    [ Agda <$ P.string "agda"
    , Idris <$ P.string "idris2"
    , Lean <$ P.string "lean"
    , Rocq <$ P.string "rocq"
    ]

-- | Render a @'Module'@ into a language.
renderLang :: Lang -> Module -> String
renderLang Agda = Agda.render
renderLang Idris = Idris.render
renderLang Lean = Lean.render
renderLang Rocq = Rocq.render

-- * Generators
--
-- $generators

-- | Options to pass to a generator.
data PanbenchOpts = PanbenchOpts
  { optSize :: Natural
  -- ^ The size of the output to generate.
  , optLang :: Lang
  -- ^ The langauge to generate for.
  }
  deriving (Show)

-- | Option parser for @PanbenchOpts@.
panbenchOptParser :: Parser PanbenchOpts
panbenchOptParser = PanbenchOpts
  <$> option auto (long "size" <> metavar "SIZE" <> help "The size of the module to generate.")
  <*> option auto (long "language" <> metavar "LANG" <> help "The language to generate for.")

-- | Construct a @panbench@ generator binary.
--
-- This function is intended to be used as the @main@ function
-- of a @panbench@ generator, and will read in a size and language
-- as @--size@ and @--language@ arguments.
panbenchMain
  :: String
  -- ^ The name of the generator.
  -> (Natural -> Module)
  -- ^ Generate a module of a given size.
  -> IO ()
panbenchMain name gen = do
  opts <- execParser $
    info (panbenchOptParser <**> helper) $ mconcat
      [ fullDesc
      , header ("panbench generator for " <> name)
      ]
  let module_ = gen (optSize opts)
  putStr (renderLang (optLang opts) module_)
