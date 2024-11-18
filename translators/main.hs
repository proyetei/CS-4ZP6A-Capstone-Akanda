module Main where

import Grammar
import PrintAgda
import PrintIdris
import PrintLean
import PrintRocq

-- this is equivalent to:
-- n :: Int
-- n = let x1 = 1 in
--     let x2 = x1 in
--     let x3 = x2 in
--         x3
test :: Module
test = Module "LetExample"
    [
        DefVar "n" (Just $ Con "Nat") (Let [DefVar "x1" Nothing $ Int 1] $ Let [DefVar "x2" Nothing $ Var "x1"] $ Let [DefVar "x3" Nothing $ Var "x2"] $ Var "x3")
    ]

main :: IO()
main = do
    runAgda test
    runIdris test
    runLean test
    runRocq test