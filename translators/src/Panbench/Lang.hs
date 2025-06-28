-- | Language metadata.
--
-- This module is intended to be imported qualified.
module Panbench.Lang
  (  -- $languages
    Lang(..)
  , render
  , name
  , fileExt
  , defaultFlags
  , defaultBinary
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
  toJSON Idris = JSON.String "idris"
  toJSON Lean = JSON.String "lean"
  toJSON Rocq = JSON.String "rocq"

-- | Render a @'Module'@ as a given language.
render :: Lang -> Module -> Text
render Agda = Agda.render
render Idris = Idris.render
render Lean = Lean.render
render Rocq = Rocq.render

-- | Get the name of a language.
name :: Lang -> String
name Agda = "agda"
name Idris = "idris"
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
defaultFlags
  :: Lang
  -- ^ The language.
  -> FilePath
  -- ^ The file we are trying to typecheck.
  -> [String]
defaultFlags Agda file = ["+RTS", "-M3.0G", "-RTS", file]
defaultFlags Idris file = ["--check", file]
defaultFlags Lean file = ["-D", "maxRecDepth=2000", "-D", "maxHeartbeats=0", file]
defaultFlags Rocq file = [file]

-- | Get the name of the binary for a language.
defaultBinary :: Lang -> String
defaultBinary Agda = "agda"
defaultBinary Idris = "idris2"
defaultBinary Lean = "lean"
defaultBinary Rocq = "coqc"

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
