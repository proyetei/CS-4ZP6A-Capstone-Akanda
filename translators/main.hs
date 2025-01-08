module Main where

import Grammar
import PrintAgda
import PrintIdris
import PrintLean
import PrintRocq

-- this is equivalent to:
-- n :: Nat
-- n = let x1 = 1 in
--     let x2 = x1 in
--     let x3 = x2 in
--         x3
test :: Module
test = Module "LetExample"
    [
        DefVar "n" (Just $ Con "Nat") (Let [DefVar "x1" Nothing $ Int 1] $ Let [DefVar "x2" Nothing $ Var "x1"] $ Let [DefVar "x3" Nothing $ Var "x2"] $ Var "x3")
    ]

-- this is equivalent to:
-- n :: Nat
-- n = let f1 x1 = x1 + 1
--         f2 x1 x2 = x1 + x2 + 1
--         f3 x1 x2 x3 = x1 + x2 + x3 + 1
--     in f1 2 + f2 2 3 + f3 2 3 4

test1 :: Module
test1 = Module "NestedFunction"
    [
        DefVar "n" (Just $ Con "Nat") (Let 
            [
                -- Define function f1 with type Nat -> Nat
                DefFun "f1" (Just $ Arr (Con "Nat") (Con "Nat")) [Arg "x1" (Con "Nat")] (Bin "+" (Var "x1") (Int 1)),
                
                -- Define function f2 with type Nat -> Nat -> Nat
                DefFun "f2" (Just $ Arr (Con "Nat") (Arr (Con "Nat") (Con "Nat"))) [Arg "x1" (Con "Nat"), Arg "x2" (Con "Nat")] (Bin "+" (Bin "+" (Var "x2") (Var "x1")) (Int 1)),
                
                -- Define function f3 with type Nat -> Nat -> Nat -> Nat
                DefFun "f3" (Just $ Arr (Con "Nat") (Arr (Con "Nat") (Arr (Con "Nat") (Con "Nat")))) [Arg "x1" (Con "Nat"), Arg "x2" (Con "Nat"), Arg "x3" (Con "Nat")] (Bin "+" (Bin "+" (Bin "+" (Var "x3") (Var "x2")) (Var "x1")) (Int 1))
            ] $ Bin "+" (Bin "+" (FunCall "f1" [Int 2]) (FunCall "f2" [Int 2, Int 3])) (FunCall "f3" [Int 2, Int 3, Int 4]) ) 
    ] 

-- DefFun "f2" Nothing [Arg "x1" Nothing, Arg "x2" Nothing] (Bin "+" (Bin "+" (Var "x1") (Var "x2") (Int 1)))
    


main :: IO()
main = do
    runAgda test1
    runIdris test1
    runLean test1
    -- runRocq test