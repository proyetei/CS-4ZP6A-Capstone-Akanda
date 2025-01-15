module Tests (tests) where

import Data.IntMap.Strict as Map
import Grammar

-- this is our list of expandable tests. each test should take an Int as an argument and return a program written in the internal grammar (see Grammar.hs)
_tests :: [Int -> Module] -- todo: add commented Haskell representation for each test
_tests = 
    [ \n -> let
            xs 0 = Int 1
            xs m = Var $ "x" ++ show m
            lets p = 
                if p==n then Let [DefVar ("x"++ show p) Nothing $ xs $ p-1] $ xs p 
                else Let [DefVar ("x"++ show p) Nothing $ xs $ p-1] $ lets $ p+1
        in Module "LetExample" [DefVar "n" (Just $ Con "Nat") $ lets 1]
    , \n -> let
        xs 0 = Int 1
        xs m = Bin " + " (Var $ "x" ++ show m) (Var $ "x" ++ show m)
        lets p = 
            if p==n then Let [DefVar ("x"++ show p) Nothing $ xs $ p-1] $ Var $ "x" ++ show p
            else Let [DefVar ("x"++ show p) Nothing $ xs $ p-1] $ lets $ p+1
    in Module "LetAddExample" [DefVar "n" (Just $ Con "Nat") $ lets 1]
    ]

-- this is the list of expandable tests formatted as an IntMap so each test can be accessed by index
-- to access the expandable test at index i: tests ! i
tests :: IntMap (Int -> Module)
tests = Map.fromList $ zip [1..(length _tests)] _tests

-- this is a list of the names of the tests, in order (to be used in the menu of CLI)
desc = fmap (\x -> (\(Module n _) -> n) $ x 0) _tests


-- add more tests below. can be expandable or not


-- this is equivalent to:
-- module LetExample where
--
-- n :: Nat
-- n = let x1 = 1 in
--     let x2 = x1 in
--     let x3 = x2 in
--         x3
test1 :: Int -> Module
test1 n = Module "LetExample" [DefVar "n" (Just $ Con "Nat") $ lets n]
    where
        xs 0 = Int 1
        xs m = Var $ "x" ++ show m
        lets 0 = Int 1
        lets p = if p==n then xs p
            else
                Let [DefVar ("x"++ show p) Nothing $ xs $ p-1] $ lets $ p+1