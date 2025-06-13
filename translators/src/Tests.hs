module Tests (tests) where

import Data.IntMap.Strict as Map ( fromList, IntMap )
import Numeric.Natural (Natural)
import GHC.Natural (minusNatural)

import Grammar

-- re-usable generators for the tests below
genIndexName :: Char -> Natural -> String
genIndexName c i = c : show i

genIndex :: Char -> Natural -> [String]
genIndex c m = map (genIndexName c) [1..m]

optional :: Natural -> [a] -> [a]
optional n c = if n == 0 then [] else c

xs :: Natural -> Expr
xs n | n == 0    = Nat 1
     | otherwise = Var $ "x" ++ show n

sum2vars :: Natural -> Expr
sum2vars n | n == 0    = Nat 1
           | otherwise = Bin " + " (Var $ "x" ++ show n) (Var $ "x" ++ show n)

-- this is our list of expandable tests. each test should take a Natural as an
-- argument and return a program written in the internal grammar (see
-- Grammar.hs)
_tests :: [Natural -> Module] -- todo: add commented Haskell representation for each test
_tests =
    [ \n -> let --1
            decl = [DefVar "n" (Just $ Con "Nat") $ lets 0]
            lets :: Natural -> Expr
            lets p = Let [DefVar ("x"++ show (p+1)) Nothing $ xs p] $ if p == minusNatural n 1 then xs (p+1) else lets (p+1)
        in Module "LetExample" [ImportLib NatMod] $ optional n decl

    , \n -> let --2
        lets p =
            if p==n then Let [DefVar ("x"++ show p) Nothing $ sum2vars $ p-1] $ Var $ "x" ++ show p
            else Let [DefVar ("x"++ show p) Nothing $ sum2vars $ p-1] $ lets $ p+1
    in Module "LetAddExample" [ImportLib NatMod] $ optional n [DefVar "n" (Just $ Con "Nat") $ lets 1]

    , -- 3 Description: Generate Nested Functions
    \n -> let --3
            iszero 0 = []
            iszero _ = [ DefVar "n" (Just $ Con "Nat") (Let (reverse(genFunc n)) (genCall n)) ]
            -- Generate function definitions dynamically based on (1 to n)
            genFunc :: Natural -> [Definition]
            genFunc 1 = [DefNesFun "f1" (Just $ Arr (Con "Nat") (Con "Nat"))
                            [Arg "x1" (Con "Nat")]
                            (Bin "+" (Var "x1") (Nat 1))]
            genFunc p = DefNesFun
                            ("f" ++ show p)  -- Function name
                            (Just $ foldr Arr (Con "Nat") (replicate (fromIntegral p) (Con "Nat")))  -- Function type
                            (map (\ i -> Arg ("x" ++ show i) (Con "Nat")) [1 .. p])  -- Function arguments
                            (foldl (\ acc i -> Bin "+" acc (Var ("x" ++ show i))) (Nat 1) [1 .. p])  -- Fixed expression
                        : genFunc (p-1)

            -- Generate function call expressions
            genCall :: Natural -> Expr
            genCall 1 = FunCall "f1" [Nat 2]
            genCall p = Bin "+"
                            (FunCall ("f" ++ show p) (map Nat [2 .. p + 1]))
                            (genCall (p - 1))

        in Module "NestedFunction" [ImportLib NatMod] $ iszero n
    , \n -> let --4 A specified number of simple datatype declarations.
        genData 0 = []
        genData 1 = [DefDataType "X1" [("Y1", Con "X1")] (Con "Type")]
        genData m = DefDataType ("X" ++ show m) [("Y" ++ show m, Con ("X" ++ show m))] (Con "Type") : genData (m-1)
        in Module "DataSimpleDeclarations" [ImportLib NatMod]  $ genData n
    , \n -> let --5 Variable declaration with an identifier of a specified length.
        iszero 0 = []
        iszero _ = [DefVar (genIdentifier n) (Just $ Con "Nat") $ Nat 0]
        genIdentifier 1 = "x"
        genIdentifier m = 'x' : genIdentifier (m-1)
        in Module "LongIdentifier" [ImportLib NatMod]  $ iszero n

    -- 6 Description: A record declaration with N dependent fields
    ,\n -> let --6
        iszero :: Natural -> [Definition]
        iszero 0 = []
        iszero _ = [xDef, exampleInit]
        -- Generate field definitions dynamically
        genFields :: Natural -> [(String, Type)]
        genFields 1 = [("f1", Con "Nat")]
        genFields 2 = genFields 1 ++ [("f2", PCon "Vec" [Con "Nat", TVar "f1"])]
        genFields p = genFields (minusNatural p 1) ++ [("f" ++ show p, PCon "Vec" [Con "Nat", genSize (minusNatural p 1)])]

        -- Helper function to correctly reference `suc` or `S`
        genSize :: Natural -> Type
        genSize 1 = TVar "f1"
        genSize p = Suc (genSize (minusNatural p 1)) -- HACK

        -- Generate example initialization dynamically
        genExample :: Natural -> [(String, Expr)]
        genExample k = map (\i -> ("f" ++ show i, buildVecCons i)) [1..k]

        -- Function to correctly build `VecCons`
        buildVecCons :: Natural -> Expr
        buildVecCons k | k == 0 = error "needs length at least 1"
        buildVecCons k          = VecE $ replicate (fromIntegral k) (Nat 1)

        -- Define the record structure
        xDef = DefRecType "Cap_X" [] "Const" (genFields n) (Con "Type")

        -- Define the example initialization
        exampleInit = DefRec "example" (Con "Cap_X") "Const" (genExample n)

        in Module "Fields_DependentRecordModule" [ImportLib NatMod, ImportLib VecMod] $ iszero n

    , --7 Description: Generate a very long chain (N) of dependent record definitions
    \n -> let
        iszero 0 = []
        iszero _ = (genRecords n ++ [exampleInit])
        -- Generate Record Definitions
        genRecords :: Natural -> [Definition]
        genRecords 1 = [DefRecType "Record1" [] "Const1" [("f1", Con "Nat")] (Con "Type")]
        genRecords level =
            let prev = "Record" ++ show (minusNatural level 1) -- HACK
                curr = "Record" ++ show level
                constructor = "Const" ++ show level
                field = "f" ++ show level
            in genRecords (minusNatural level 1) ++ [DefRecType curr [] constructor [(field, Con prev)] (Con "Type")]

        -- Generate Example Init
        genExample :: Natural -> Expr
        genExample 0 = Nat 10
        genExample 1 = Paren $ FunCall "Const1" [Nat 10]
        genExample level =
            let prevExample = genExample (minusNatural level 1) -- HACK
                constructor = "Const" ++ show level
            in Paren $ FunCall constructor [prevExample]

        exampleInit = DefRec "example" (Con $ "Record" ++ show n) ("Const" ++ show n) [("example", genExample $ minusNatural n 1)] -- HACK

        in Module "ChainDef_DependentRecordModule" [ImportLib NatMod] $ iszero n

    , -- 8 Description: Generate record with N parameters
    \n -> let
        iszero 0 = []
        iszero _ = [recDef, exampleInit]
        -- Helper to build the sum exp 1 + 2 + ... + n
        buildSum 1 = Nat 1
        buildSum m = Bin "+" (buildSum (m-1)) (Nat m)
        sumExpr = buildSum n

        -- Create param as a list of Args: f1 : Nat, f2 : Nat, …, fn : Nat
        params = map (\i -> Arg ("f" ++ show i) (Con "Nat")) [1..n]

        -- Define the record X with param, a constructor "Const",
        -- two fields "sums" and "values", and overall type Set.
        recDef = DefRecType "X" params "Const"
                 [("sums", Con "Nat")]
                 (Con "Set")

        -- Build the record type application as a string: "X 1 2 ... n"
        -- recTypeInstance = "X " ++ unwords (map show [1..n])
        recTypeInstance = DCon "X" [] $ map Nat [1..n]

        -- Define the record instance "example" with computed field values:
        exampleInit = DefRec "example" recTypeInstance "Const"
                       [("sums", Paren sumExpr)]
      in Module "Parameters_DependentRecordModule" [ImportLib NatMod] $ iszero n
    , \n -> let -- 9
    -- Generate a file with n newlines where n = user input
    newlines = replicate (fromIntegral n) '\n'
    in File "NewlineFile" newlines

    , \n -> let -- 10 Description: A record declaration with N independent fields
        iszero 0 = []
        iszero _ = [xDef,exampleInit]
        -- Generate field definitions dynamically
        genFields 1 = [("f1", Con "Nat")]
        genFields p = genFields (p - 1) ++ [("f" ++ show p, Con "Nat")]
        -- Define the record structure
        xDef = DefRecType "Cap_X" [] "Const" (genFields n) (Con "Type")
        -- Generate example initialization dynamically
        genExample 1 = [("f1", Nat 1)]
        genExample p = genExample (p - 1) ++ [("f" ++ show p, Nat 1)]
        -- Define the example initialization
        exampleInit = DefRec "example" (Con "Cap_X") "Const" (genExample n)
    in Module "Fields_NonDependentRecordModule" [ImportLib NatMod] $ iszero n

    , \n -> let -- 11 Description: Generate a very long chain (N) of independent record definitions
        iszero 0 = []
        iszero _ = (genRecords n ++ [exampleInit])
        -- Generate Record Definitions
        genRecords 0 = []
        genRecords 1 = [DefRecType "Record1" [] "Const1" [("f1", Con "Nat")] (Con "Type")]
        genRecords p = genRecords (p - 1) ++ [DefRecType ("Record" ++ show p) [] ("Const" ++ show p) [("f" ++ show p, Con "Nat")] (Con "Type")]
        --just "" to prevent inheretence of DefRecType
        exampleInit = DefRec "example" (Con $ "Record" ++ show n) ("Const" ++ show n) [("f1", Nat 1)]
    in Module "ChainDefFields_NonDependentRecordModule" [ImportLib NatMod] $ iszero n

    , \n -> --12 Description: create a simple datatype with N constructors accepting no parameters
        let
            iszero 0 = []
            iszero _ = [DefDataType "D" (map (\ i -> ("C" ++ show i, Con "D")) [1 .. n]) (Con "Type")]
        in Module "Constructors_Datatypes" [] $ iszero n

    , \n ->  --13 Description: creates a datatype with a single constructor accepting N parameters
        let
            iszero 0 = []
            iszero _ = [DefPDataType "D" (map (\i -> ("p" ++ show i, Con "Type")) [1 .. n]) [("C", PCon "D" (map (\i -> Con ("p" ++ show i) ) [1 .. n]))] (Con "Type")]
        in Module "Parameters_Datatypes" [] $ iszero n

    , --14 Description: defines N variables, and uses both the first and last one in a declaration, N>=2
     \n ->
    let
        iszero 0 = []
        iszero _ = (varDefs ++ [resultDef])
        -- Generate variable names: x1, x2, ..., xn
        varNames = map (\i -> "x" ++ show i) [1..n]

        -- Generate definitions: x1 = 1, x2 = 2, ..., xn = n
        varDefs = zipWith (\name val -> DefVar name (Just $ Con "Nat") (Nat val)) varNames [1..n]

        -- result = x1 + xn
        finalExpr = Bin "+" (Var "x1") (Var ("x" ++ show n))
        resultDef = DefVar "result" (Just $ Con "Nat") finalExpr

    in Module "FirstLast_VariableModule" [ImportLib NatMod] $ iszero n
    , -- 15 Description: defines lots of dependent variables (10 at each level of dependency) and then use the most nested ones in a declaration
    \n -> let
    iszero 0 = []
    iszero _ = (genLevelDefs n ++ [resultDef])
    varsPerLevel = 10  -- Number of variables per level

    -- Generate variable names format x$level$ L $index$
    varName :: Natural -> Natural -> String
    varName level idx = "x" ++ show level ++ "L" ++ show idx

    -- Define expressions for each variable
    genExpr :: Natural -> Natural -> Expr
    genExpr 1 idx = Nat idx
    genExpr level idx = Bin "+" (Var $ varName (level - 1) idx) (Nat idx)

    -- Generate DefVar for each level
    genLevelDefs :: Natural -> [Definition]
    genLevelDefs 1 = [DefVar (varName 1 idx) (Just $ Con "Nat") (genExpr 1 idx) | idx <- [1..varsPerLevel]]
    genLevelDefs level = genLevelDefs (level - 1) ++
        [DefVar (varName level idx) (Just $ Con "Nat") (genExpr level idx) | idx <- [1..varsPerLevel]]

    --  sum of all xN_1 .. xN_10 + 100
    sumVars = foldl (\acc idx -> Bin "+" acc (Var $ varName n idx)) (Nat 100) [1..varsPerLevel]

    resultDef = DefVar "result" (Just $ Con "Nat") sumVars

    in Module "DeepDependency_VariableModule" [ImportLib NatMod] $ iszero n
    , \n -> let -- 16 Description: Simple datatype declaration with a specified number of indices, defined implicitly.
        iszero 0 = []
        iszero _ = [DefDataType "D" [("C1", Arr (Index (genIndex 'x' n) (Con "Nat")) (Con ("D" ++ " " ++ unwords (genIndex 'x' n))))] (Arr (genType n) (Con "Type"))]
        genType 1 = Con "Nat"
        genType m = Arr (genType (m-1)) (Con "Nat")

       in Module "DataImplicitIndices" [ImportLib NatMod] $ iszero n
    , \n -> let -- 17 Description: A file consisting of a single long line (length specified by the user).
        iszero 0 = []
        iszero _ = [DefVar "A" (Just (Con "String")) $ String (genLongValue n)]
        genLongValue 1 = "x"
        genLongValue m = 'x' : genLongValue (m-1)
        in Module "SingleLongLine" [ImportLib StringMod]  $ iszero n
    , \n ->  --18 Description: A single datatype where 'n' represents the number of 'Type' parameters, all needed for 'n' constructors
        let
            iszero 0 = []
            iszero _ =  [DefPDataType "D" (map (\i -> ("p" ++ show i, Con "Type")) [1 .. n]) (map (\ i -> ("C" ++ show i,  PCon "D" (map (\j -> Con ("p" ++ show j) ) [1 .. n]))) [1 .. n]) (Con "Type")]
        in Module "ConstructorsParameters_Datatypes" [] $ iszero n
    , \n -> let -- 19  Description: A single datatype where 'n' represents the number of indices, all needed for 'n' constructors
        iszero 0 = []
        iszero _ = [DefDataType "D"
           (map (\ i -> ("C" ++ show i, Arr (Index (genIndex 'x' i) (Con "Nat")) (PCon "D" ((map (\j -> Con ("X" ++ show j)) [1 .. i]) ++ map (\_ -> Con "0") [i+1..n])))) [1 .. n]) (Arr (genType n) (Con "Type"))]
        genType 1 = Con "Nat"
        genType m = Arr (genType (m-1)) (Con "Nat")

        in Module "IndicesConstructors_Datatypes" [ImportLib NatMod] $ iszero n
    , \n -> let -- 20  Description: A single datatype where 'n' represents the number of 'Type' parameters as well as the number of indices
        iszero 0 = []
        iszero _ = [DefPDataType "D"
          (map (\i -> ("p" ++ show i, Con "Type")) [1 .. n])
          [("C", Arr (Index (genIndex 'X' n) (Con "Nat")) (PCon "D" ((map (\i -> Con ("p" ++ show i))  [1 .. n]) ++ map (\j -> Con ("X" ++ show j)) [1 .. n])))]
          (Arr (genType n) (Con "Type"))]
        genType 1 = Con "Nat"
        genType m = Arr (genType (m-1)) (Con "Nat")

        in Module "IndicesParameters_Datatypes" [ImportLib NatMod] $ iszero n
    ,  \n -> --21 Description: A function pattern matching on 'n' constructors of a datatype
        let
        iszero 0 = []
        iszero _ = [DefDataType "D" (map (\ i -> ("C" ++ show i, Con "D")) [1 .. n]) (Con "Type"), --create datatype
          OpenName "D",
          DefPatt "F" [("C", Con "D")] (Con "Nat") "C" (map (\i -> ([Arg ("C" ++ show i) (Con "D")], Nat i)) [1..n]),
          DefVar "N" (Just $ Con "Nat") (genCall n)]
        genCall 1 = FunCall "F" [Constructor "C1"]
        genCall p = Bin "+" (FunCall "F" [Constructor ("C" ++ show p)]) (genCall (p-1))
    in
       Module "Pattern_Matching_Datatypes" [ImportLib NatMod] $ iszero n
     ]


-- this is the list of expandable tests formatted as an IntMap so each test can be accessed by index
-- to access the expandable test at index i: tests ! i
tests :: IntMap (Natural -> Module)
tests = Map.fromList $ zip [1..(length _tests)] _tests
