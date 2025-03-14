module Tests (tests) where
import Data.List (intercalate)
import Data.IntMap.Strict as Map ( fromList, IntMap )
import Grammar

-- this is our list of expandable tests. each test should take an Int as an argument and return a program written in the internal grammar (see Grammar.hs)
_tests :: [Int -> Module] -- todo: add commented Haskell representation for each test
_tests = 
    [ \n -> let --1
            xs 0 = Int 1
            xs m = Var $ "x" ++ show m
            lets p = 
                if p==n then Let [DefVar ("x"++ show p) Nothing $ xs $ p-1] $ xs p 
                else Let [DefVar ("x"++ show p) Nothing $ xs $ p-1] $ lets $ p+1
        in Module "LetExample" [DefVar "n" (Just $ Con "Nat") $ lets 1]
    , \n -> let --2
        xs 0 = Int 1
        xs m = Bin " + " (Var $ "x" ++ show m) (Var $ "x" ++ show m)
        lets p = 
            if p==n then Let [DefVar ("x"++ show p) Nothing $ xs $ p-1] $ Var $ "x" ++ show p
            else Let [DefVar ("x"++ show p) Nothing $ xs $ p-1] $ lets $ p+1
    in Module "LetAddExample" [DefVar "n" (Just $ Con "Nat") $ lets 1]
    , \n -> let --3
            -- Generate function definitions dynamically based on (1 to n)
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
    , \n -> let --4
        genData 1 = [DefDataType "x1" [("y", Con "Bool")] (Con "Type")]
        genData m = DefDataType ("x" ++ show m) [("y", Con "Bool")] (Con "Type") : genData (m-1)
        in Module "DataSimpleDeclarations" (genData n)
    , \n -> let --5
        genIdentifier 1 = "x"
        genIdentifier m = 'x' : genIdentifier (m-1)
        in Module "LongIdentifier" [DefVar (genIdentifier n) (Just $ Con "Nat") $ Int 0]
    ,\n -> let --6
        -- Generate field definitions dynamically
        genFields 1 = [("f1", Con "Nat")]
        genFields 2 = genFields 1 ++ [("f2", PCon "Vec" [Con "Nat", TVar "f1"])]
        genFields p = genFields (p - 1) ++ [("f" ++ show p, PCon "Vec" [Con "Nat", genSize (p - 1)])]

        -- Helper function to correctly reference `suc` or `S`
        genSize 1 = TVar "f1"
        genSize p = Suc (genSize (p - 1))

        -- Generate example initialization dynamically
        genExample 1 = [("f1", Int 1)]
        genExample 2 = genExample 1 ++ [("f2", VecCons (Int 1) VecEmpty)]
        genExample p = genExample (p - 1) ++ [("f" ++ show p, buildVecCons (p - 1))] 

        -- Function to correctly build `VecCons`
        buildVecCons 1 = VecCons (Int 1) VecEmpty
        buildVecCons p = VecCons (Int 1) (buildVecCons (p - 1))

        -- Define the record structure
        xDef = DefRecType "X" Nothing Nothing (genFields n) (Con "Set")

        -- Define the example initialization
        exampleInit = InitRec "example" "X" Nothing (genExample n)

        in Module "Fields_DependentRecordModule" [xDef, exampleInit]
    , \n -> let --7
        -- Generate Record Definitions
        genRecords 1 = [DefRecType "Record1" Nothing (Just "Const1") [("f1", Con "Nat")] (Con "Set")]
        genRecords level =
            let prev = "Record" ++ show (level - 1)
                curr = "Record" ++ show level
                constructor = "Const" ++ show level 
                field = "f" ++ show level
            in genRecords (level - 1) ++ [DefRecType curr Nothing (Just constructor) [(field, Con prev)] (Con "Set")]

        -- Generate Example Init
        genExample 1 = FunCall "Const1" [Int 10] 
        genExample level =
            let prevExample = genExample (level - 1)
                constructor = "Const" ++ show level 
            in FunCall constructor [Paren prevExample] 

        --just "" to prevent inheretence of DefRecType
        exampleInit = InitRec "example" ("Record" ++ show n) (Just ("")) [("example", genExample n)]
        
        in Module "ChainDef_DependentRecordModule" (genRecords n ++ [exampleInit])
    , \n -> let --8
         -- Helper to build the sum exp 1 + 2 + ... + n
         buildSum 1 = Int 1
         buildSum m = Bin "+" (buildSum (m-1)) (Int m)
         sumExpr = buildSum n

         -- Helper to build the list exp: [1, 2, …, n]
         buildList i = if i > n then ListEmpty else ListCons (Int i) (buildList (i+1))
         listExpr = buildList 1

         -- Create param as a list of Args: f1 : Nat, f2 : Nat, …, fn : Nat
         params = Just $ map (\i -> Arg ("f" ++ show i) (Con "Nat")) [1..n]

         -- Define the record X with param, a constructor "Const",
         -- two fields "sums" and "values", and overall type Set.
         recDef = DefRecType "X" params (Just "Const")
                  [("sums", Con "Nat"), ("values", PCon "List" [Con "Nat"])]
                  (Con "Set")

         -- Build the record type application as a string: "X 1 2 ... n"
         recTypeInstance = "X " ++ unwords (map show [1..n])

         -- Define the record instance "example" with computed field values:
         exampleInit = InitRec "example" recTypeInstance (Just "Const")
                        [("sums", Paren sumExpr), ("values", listExpr)]
       in Module "Parameters_DependentRecordModule" [recDef, exampleInit]
    , \n -> let -- 9
    -- Generate a file with n newlines where n = user input
    newlines = replicate n '\n'
    in File "NewlineFile" newlines -- Use the file constructor defined in Grammar.hs
    , \n -> let -- 10
        -- Generate field definitions dynamically
        genFields 1 = [("f1", Con "Nat")]
        genFields p = genFields (p - 1) ++ [("f" ++ show p, Con "Nat")]
        -- Define the record structure
        xDef = DefRecType "X" Nothing Nothing (genFields n) (Con "Set")
        -- Generate example initialization dynamically
        genExample 1 = [("f1", Int 1)]
        genExample p = genExample (p - 1) ++ [("f" ++ show p, Int 1)] 
        -- Define the example initialization
        exampleInit = InitRec "example" "X" Nothing (genExample n)
    in Module "Fields_NonDependentRecordModule" [xDef,exampleInit]
    , \n -> let -- 11
        -- Generate Record Definitions
        genRecords 1 = [DefRecType "Record1" Nothing (Just "Const1") [("f1", Con "Nat")] (Con "Set")]
        genRecords p = genRecords (p - 1) ++ [DefRecType ("Record" ++ show p) Nothing (Just $ "Const" ++ show p) [("f" ++ show p, Con "Nat")] (Con "Set")]
        --just "" to prevent inheretence of DefRecType
        exampleInit = InitRec "example" ("Record" ++ show n) Nothing [("f1", Int 1)]
    in Module "ChainDefFields_NonDependentRecordModule" (genRecords n ++ [exampleInit])
    , \n -> let -- 12
        genType 1 = Con "Nat"
        genType m = Arr (genType (m-1)) (Con "Nat")

        genIndexName i = 'x' : show i
        
        genIndex 1 = [genIndexName 1]
        genIndex m = genIndexName m : genIndex (m-1)
       in Module "DataImplicitIndices" [DefDataType "d" [("c1", Arr (Index (genIndex n) (Con "Nat")) (Con "d"))] (Arr (genType n) (Con "Type"))]
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
    