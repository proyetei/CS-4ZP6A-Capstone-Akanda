{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Print.Generic where

-- Utilities to be shared amongst all Print modules
parens :: String -> String

parens e = "(" ++ e ++ ")"

