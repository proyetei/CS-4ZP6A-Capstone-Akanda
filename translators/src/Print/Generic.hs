{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Print.Generic where

import Numeric.Natural (Natural)

import Prettyprinter

-- Utilities to be shared amongst all Print modules
blanklines :: Natural -> Doc ann
blanklines n = vcat $ replicate (fromIntegral n) emptyDoc

-- (soon to be obsolete) Utilities to be shared amongst all Print modules
parens, brackets, sqbrackets, quote, line :: String -> String
parens e = "(" ++ e ++ ")"
brackets e = "{" ++ e ++ "}"
sqbrackets e = "[" ++ e ++ "]"
quote e = "\"" ++ e ++ "\""
line e = e ++ "\n"

