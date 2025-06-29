-- | Language metadata.
--
-- This module is intended to be imported qualified.
module Panbench.Lang
  (  -- $languages
    Lang(..)
  , render
  , name
  , fileExt
  , defaultArgs
  , defaultExecutable
  , buildArtifacts
  , allLangs
  ) where

import Control.DeepSeq

import Data.Binary
import Data.Functor
import Data.Hashable
import Data.Map (Map)
import Data.Text (Text)

import Data.Aeson qualified as JSON
import Data.Map.Strict qualified as Map

import GHC.Generics

import Text.ParserCombinators.ReadP qualified as Read
import Text.Read

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
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (Hashable, Binary, NFData)

instance Read Lang where
  readPrec = lift $ Read.choice
    [ Agda <$ Read.string "agda"
    , Idris <$ Read.string "idris2"
    , Lean <$ Read.string "lean"
    , Rocq <$ Read.string "rocq"
    ]

instance JSON.ToJSON Lang where
  toJSON Agda = JSON.String "agda"
  toJSON Idris = JSON.String "idris2"
  toJSON Lean = JSON.String "lean"
  toJSON Rocq = JSON.String "rocq"

instance JSON.FromJSON Lang where
  parseJSON = JSON.withText "Lang" \case
    "agda" -> pure Agda
    "idris2" -> pure Idris
    "lean" -> pure Lean
    "rocq" -> pure Rocq
    _ -> fail "Expected one of agda, idris2, lean, rocq."

-- | Render a @'Module'@ as a given language.
render :: Lang -> Module -> Text
render Agda = Agda.render
render Idris = Idris.render
render Lean = Lean.render
render Rocq = Rocq.render

-- | Get the name of a language.
name :: Lang -> String
name Agda = "agda"
name Idris = "idris2"
name Lean = "lean"
name Rocq = "rocq"

-- | Get the extension type for a given @Lang@.
fileExt :: Lang -> String
fileExt Agda = ".agda"
fileExt Idris = ".idr"
fileExt Lean = ".lean"
fileExt Rocq = ".v"

-- | Get the default flags to use for a @'Lang'@ when
-- trying to typecheck a file.
defaultArgs
  :: Lang
  -- ^ The language.
  -> FilePath
  -- ^ The file we are trying to typecheck.
  -> [String]
defaultArgs Agda file = ["+RTS", "-M3.0G", "-RTS", file]
defaultArgs Idris file = ["--check", file]
defaultArgs Lean file = ["-D", "maxRecDepth=2000", "-D", "maxHeartbeats=0", file]
defaultArgs Rocq file = [file]

-- | Get the name of the binary for a language.
defaultExecutable :: Lang -> String
defaultExecutable Agda = "agda"
defaultExecutable Idris = "idris2"
defaultExecutable Lean = "lean"
defaultExecutable Rocq = "coqc"

-- | Build artifacts produced by a given @'Lang'@.
buildArtifacts :: Lang -> [FilePath]
buildArtifacts Agda = ["*.agdai"]
buildArtifacts Idris = ["build/*"]
buildArtifacts Lean = []
buildArtifacts Rocq = ["*.vo", "*.vok", "*.vos", "*.glob"]

-- | A @'Map'@ containing all currently supported languages, keyed by
-- the language name.
allLangs :: Map String Lang
allLangs =
  Map.fromList $ [minBound..maxBound] <&> \lang ->
    (name lang, lang)
