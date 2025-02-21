module Tests (tests) where
import Data.List (intercalate)
import Data.IntMap.Strict as Map ( fromList, IntMap )
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
    , \n -> let
            -- Generate function definitions dynamically based on arity (1 to n)
            genFunc 1 = [DefNesFun "f1" (Just $ Arr (Con "Nat") (Con "Nat"))
                            [Arg "x1" (Con "Nat")] 
                            (Bin "+" (Var "x1") (Int 1))]
            genFunc p = DefNesFun
                            ("f" ++ show p)  -- Function name
                            (Just $ foldr Arr (Con "Nat") (replicate p (Con "Nat")))  -- Function type
                            (map (\ i -> Arg ("x" ++ show i) (Con "Nat")) [1 .. p])  -- Function arguments
                            (foldl (\ acc i -> Bin "+" acc (Var ("x" ++ show i))) (Int 1) [1 .. p])  -- Fixed expression
                        : genFunc (p-1)

            -- Generate function call expressions
            genCall 1 = FunCall "f1" [Int 2]
            genCall p = Bin "+" 
                            (FunCall ("f" ++ show p) (map Int [2 .. p + 1])) 
                            (genCall (p - 1))

        in Module "NestedFunction" 
            [ DefVar "n" (Just $ Con "Nat") 
                (Let (reverse(genFunc n)) (genCall n)) ]
    ,\n -> let
        -- Generate field definitions dynamically
        genFields 1 = [("f1", Con "Nat")]  -- Base case
        genFields 2 = genFields 1 ++ [("f2", PCon "Vec" [Con "Nat", TVar "f1"])]
        genFields p = genFields (p - 1) ++ [("f" ++ show p, PCon "Vec" [Con "Nat", genSize (p - 1)])]

        -- Helper function to correctly reference `suc` or `S`
        genSize 1 = TVar "f1"
        genSize p = Suc (genSize (p - 1))

        -- Generate example initialization dynamically
        genExample 1 = [("f1", Int 1)]  -- Base case
        genExample 2 = genExample 1 ++ [("f2", VecCons (Int 1) VecEmpty)]
        genExample p = genExample (p - 1) ++ [("f" ++ show p, buildVecCons (p - 1))]  -- 🔹 Adjusted Here

        -- Function to correctly build `VecCons` with the right number of elements
        buildVecCons 1 = VecCons (Int 1) VecEmpty
        buildVecCons p = VecCons (Int 1) (buildVecCons (p - 1))  -- 🔹 Adjusted Here

        -- Define the record structure
        xDef = DefRecType "X" Nothing (genFields n) (Con "Set")

        -- Define the example initialization
        exampleInit = InitRec "example" "X" (genExample n)

        in Module "DependentRecordModule" [xDef, exampleInit]
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


-- test1 :: Int -> Module
-- test1 n = Module "LetExample" [DefVar "n" (Just $ Con "Nat") $ lets n]
--     where
--         xs 0 = Int 1
--         xs m = Var $ "x" ++ show m
--         lets 0 = Int 1
--         lets p = if p==n then xs p
--             else
--                 Let [DefVar ("x"++ show p) Nothing $ xs $ p-1] $ lets $ p+1


-- this is equivalent to:
-- n :: Nat
-- n = let f1 x1 = x1 + 1
--         f2 x1 x2 = x1 + x2 + 1
--         f3 x1 x2 x3 = x1 + x2 + x3 + 1
--     in f1 2 + f2 2 3 + f3 2 3 4

-- test2 :: Module
-- test2 = Module "NestedFunction"
--     [
--         DefVar "n" (Just $ Con "Nat") (Let 
--             [
--                 -- Define function f1 with type Nat -> Nat
--                 DefNesFun "f1" (Just $ Arr (Con "Nat") (Con "Nat")) [Arg "x1" (Con "Nat")] (Bin "+" (Var "x1") (Int 1)),
                
--                 -- Define function f2 with type Nat -> Nat -> Nat
--                 DefNesFun "f2" (Just $ Arr (Con "Nat") (Arr (Con "Nat") (Con "Nat"))) [Arg "x1" (Con "Nat"), Arg "x2" (Con "Nat")] (Bin "+" (Bin "+" (Var "x2") (Var "x1")) (Int 1)),
                
--                 -- Define function f3 with type Nat -> Nat -> Nat -> Nat
--                 DefNesFun "f3" (Just $ Arr (Con "Nat") (Arr (Con "Nat") (Arr (Con "Nat") (Con "Nat")))) [Arg "x1" (Con "Nat"), Arg "x2" (Con "Nat"), Arg "x3" (Con "Nat")] (Bin "+" (Bin "+" (Bin "+" (Var "x3") (Var "x2")) (Var "x1")) (Int 1))
--             ] $ Bin "+" (Bin "+" (FunCall "f1" [Int 2]) (FunCall "f2" [Int 2, Int 3])) (FunCall "f3" [Int 2, Int 3, Int 4]) ) 
--     ] 

-- DefFun "f2" Nothing [Arg "x1" Nothing, Arg "x2" Nothing] (Bin "+" (Bin "+" (Var "x1") (Var "x2") (Int 1)))
    