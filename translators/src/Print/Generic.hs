{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Print.Generic where

-- Utilities to be shared amongst all Print modules
import Prettyprinter hiding (parens, brackets, line)
import Numeric.Natural (Natural)

-- Utilities to be shared amongst all Print modules
prettyArgs :: Pretty b => b -> (a -> Doc ann) -> [a] -> Doc ann
prettyArgs var pr args = if null args then pretty var else pretty var <+> hsep (map pr args)

blanklines :: Natural -> Doc ann
blanklines n = vcat $ replicate (fromIntegral n) emptyDoc

-- (soon to be obsolete) Utilities to be shared amongst all Print modules
parens, brackets, sqbrackets, quote, line :: String -> String
parens e = "(" ++ e ++ ")"
brackets e = "{" ++ e ++ "}"
sqbrackets e = "[" ++ e ++ "]"
quote e = "\"" ++ e ++ "\""
line e = e ++ "\n"

