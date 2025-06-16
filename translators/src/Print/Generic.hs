{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Print.Generic where

-- Utilities to be shared amongst all Print modules
parens, brackets, sqbrackets, quote, line :: String -> String
parens e = "(" ++ e ++ ")"
brackets e = "{" ++ e ++ "}"
sqbrackets e = "[" ++ e ++ "]"
quote e = "\"" ++ e ++ "\""
line e = e ++ "\n"

