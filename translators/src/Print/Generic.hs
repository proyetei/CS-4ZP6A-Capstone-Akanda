{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Print.Generic where

-- Utilities to be shared amongst all Print modules
parens, brackets, quote :: String -> String
parens e = "(" ++ e ++ ")"
brackets e = "{" ++ e ++ "}"
quote e = "\"" ++ e ++ "\""

