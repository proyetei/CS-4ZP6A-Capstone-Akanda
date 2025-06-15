module Tests (tests) where

import Data.IntMap.Strict as Map ( fromList, IntMap )
import Numeric.Natural (Natural)
import GHC.Natural (minusNatural)

import Grammar

-- re-usable generators for the tests below

-- the 'n == 0' case is often trivial
trivial :: Natural -> [a] -> [a]
trivial n c = if n == 0 then [] else c

-- there is likely a nicer way to say this?
-- (The code is 1-based, keep that for now)
iter :: Natural -> (Natural -> a) -> [a]
iter n f = map f [1..n]

-- various name and variable generators
nm :: Char -> Natural -> String
nm c n = c : show n

genIndex :: Char -> Natural -> [String]
genIndex c m = map (nm c) [1..m]

v :: Char -> Natural -> Expr
v c n = Var $ nm c n

vx :: Natural -> Expr
vx = v 'x'

xs :: Natural -> Expr
xs n | n == 0    = Nat 1
     | otherwise = vx n

sum2vars :: Natural -> Expr
sum2vars n | n == 0    = Nat 1
           | otherwise = Bin " + " (vx n) (vx n)

-- this is our list of expandable tests. each test should take a Natural as an
-- argument and return a program written in the internal grammar (see
-- Grammar.hs)
_tests :: [Natural -> Module] -- todo: add commented Haskell representation for each test
_tests =
    [ \n -> let --1
            decl = [DefTVar "n" nat $ lets n]
            lets p = foldr (\a b -> Let [DefUVar (nm 'x' a) $ xs (a-1)] b) (xs p) $ [1..p]
        in Module "LetExample" [ImportLib NatMod] $ trivial n decl

    , \n -> let --2
        lets p = foldr (\a b -> Let [DefUVar (nm 'x' a) $ sum2vars $ a-1] b) (vx p) [1..p]
    in Module "LetAddExample" [ImportLib NatMod] $ trivial n [DefTVar "n" nat $ lets n]

    , -- 3 Description: Generate Nested Functions
    \n -> let --3
            decl = [ DefTVar "n" nat (Let (reverse $ genFunc n) (genCall n)) ]
            -- Generate function definitions dynamically based on (1 to n)
            genFunc :: Natural -> [Definition]
            genFunc p = foldr (\a b -> DefNesFun (nm 'f' a) 
                                       (Just $ foldr Arr nat (replicate (fromIntegral a) nat)) 
                                       (iter a (\i -> Arg (nm 'x' i) nat))
                                       (foldl (\acc i -> Bin "+" acc (vx i)) (Nat 1) [1..a]) : b)
                              []
                              $ reverse [1..p]

            -- Generate function call expressions
            genCall :: Natural -> Expr
            genCall p = foldr (\a b -> Bin "+" (FunCall (nm 'f' a) (iter a (Nat . (+ 1)))) b)
                              (FunCall "f1" [Nat 2]) $ reverse [2..p]

        in Module "NestedFunction" [ImportLib NatMod] $ trivial n decl
    , \n -> let --4 A specified number of simple datatype declarations.
      genData m = foldl (\b a -> DefDataType (nm 'X' a) [(nm 'Y' a, Con (nm 'X' a))] Univ : b) [] [1..m]
        in Module "DataSimpleDeclarations" [ImportLib NatMod]  $ genData n
    , \n ->     --5 Variable declaration with an identifier of a specified length.
           Module "LongIdentifier" [ImportLib NatMod]  $ trivial n [DefTVar (replicate (fromIntegral n) 'x') nat $ Nat 0]

    -- 6 Description: A record declaration with N dependent fields
    ,\n -> let --6
        -- Generate field definitions dynamically
        genFields :: Natural -> [(String, Type)]
        genFields p = ("f1", nat) :
                      (foldr (\a b -> (nm 'f' a, PCon "Vec" [nat, genSize (a-1)]) : b) [] $ [2..p])

        -- Helper function to correctly reference `suc` or `S`
        genSize p = foldr (\_ b -> Suc b) (TVar "f1") [2..p]

        -- Generate example initialization dynamically
        genExample :: Natural -> [(String, Expr)]
        genExample k = map (\i -> (nm 'f' i, VecE $ replicate (fromIntegral i) (Nat 1))) [1..k]

        -- Define the record structure
        xDef = DefRecType "Cap_X" [] "Const" (genFields n) Univ

        -- Define the example initialization
        exampleInit = DefRec "example" (Con "Cap_X") "Const" (genExample n)

        decl = [xDef, exampleInit]
        in Module "Fields_DependentRecordModule" [ImportLib NatMod, ImportLib VecMod] $ trivial n decl

    , --7 Description: Generate a very long chain (N) of dependent record definitions
    \n -> let
        decl = (genRecords n ++ [exampleInit])
        -- Generate Record Definitions
        genRecords :: Natural -> [Definition]
        genRecords p = foldr (\(i,c) b -> DefRecType ("Record" ++ show i) [] ("Const" ++ show i) 
          [(nm 'f' i, c)] Univ : b) [] $ ((1,Con "Nat") : map (\i -> (i, Con ("Record" ++ show (i-1)))) [2..p])

        -- Generate Example Init
        genExample :: Natural -> Expr
        genExample p = foldr (\a b -> Paren $ (FunCall ("Const" ++ show a) [b])) (Nat 10) $ reverse [1..p]

        exampleInit = DefRec "example" (Con $ "Record" ++ show n) ("Const" ++ show n) [("example", genExample $ minusNatural n 1)] -- HACK

        in Module "ChainDef_DependentRecordModule" [ImportLib NatMod] $ trivial n decl

    , -- 8 Description: Generate record with N parameters
    \n -> let
        decl = [recDef, exampleInit]
        -- Helper to build the sum exp 1 + 2 + ... + n
        buildSum 1 = Nat 1
        buildSum m = Bin "+" (buildSum (m-1)) (Nat m)
        sumExpr = buildSum n

        -- Create param as a list of Args: f1 : Nat, f2 : Nat, …, fn : Nat
        params = map (\i -> Arg ("f" ++ show i) nat) [1..n]

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
      in Module "Parameters_DependentRecordModule" [ImportLib NatMod] $ trivial n decl
    , \n -> -- 9
        -- Generate a file with n newlines where n = user input
        File "NewlineFile" $ replicate (fromIntegral n) '\n'

    , \n -> let -- 10 Description: A record declaration with N independent fields
        decl = [xDef,exampleInit]
        -- Generate field definitions dynamically
        genFields 1 = [("f1", Con "Nat")]
        genFields p = genFields (p - 1) ++ [("f" ++ show p, Con "Nat")]
        -- Define the record structure
        xDef = DefRecType "Cap_X" [] "Const" (genFields n) Univ
        -- Generate example initialization dynamically
        genExample 1 = [("f1", Nat 1)]
        genExample p = genExample (p - 1) ++ [("f" ++ show p, Nat 1)]
        -- Define the example initialization
        exampleInit = DefRec "example" (Con "Cap_X") "Const" (genExample n)
    in Module "Fields_NonDependentRecordModule" [ImportLib NatMod] $ trivial n decl

    , \n -> let -- 11 Description: Generate a very long chain (N) of independent record definitions
        decl = (genRecords n ++ [exampleInit])
        -- Generate Record Definitions
        genRecords 0 = []
        genRecords 1 = [DefRecType "Record1" [] "Const1" [("f1", Con "Nat")] Univ]
        genRecords p = genRecords (p - 1) ++ [DefRecType ("Record" ++ show p) [] ("Const" ++ show p) [("f" ++ show p, Con "Nat")] Univ]
        --just "" to prevent inheretence of DefRecType
        exampleInit = DefRec "example" (Con $ "Record" ++ show n) ("Const" ++ show n) [("f1", Nat 1)]
    in Module "ChainDefFields_NonDependentRecordModule" [ImportLib NatMod] $ trivial n decl

    , \n -> --12 Description: create a simple datatype with N constructors accepting no parameters
        let
            decl = [DefDataType "D" (map (\ i -> ("C" ++ show i, Con "D")) [1 .. n]) Univ]
        in Module "Constructors_Datatypes" [] $ trivial n decl

    , \n ->  --13 Description: creates a datatype with a single constructor accepting N parameters
        let
            decl = [DefPDataType "D" (map (\i -> ("p" ++ show i, Univ)) [1 .. n]) [("C", PCon "D" (map (\i -> Con ("p" ++ show i) ) [1 .. n]))] Univ]
        in Module "Parameters_Datatypes" [] $ trivial n decl

    , --14 Description: defines N variables, and uses both the first and last one in a declaration, N>=2
     \n ->
    let
        decl = (varDefs ++ [resultDef])
        -- Generate variable names: x1, x2, ..., xn
        varNames = map (\i -> "x" ++ show i) [1..n]

        -- Generate definitions: x1 = 1, x2 = 2, ..., xn = n
        varDefs = zipWith (\name val -> DefTVar name nat (Nat val)) varNames [1..n]

        -- result = x1 + xn
        finalExpr = Bin "+" (Var "x1") (vx n)
        resultDef = DefTVar "result" nat finalExpr

    in Module "FirstLast_VariableModule" [ImportLib NatMod] $ trivial n decl
    , -- 15 Description: defines lots of dependent variables (10 at each level of dependency) and then use the most nested ones in a declaration
    \n -> let
    decl = (genLevelDefs n ++ [resultDef])
    varsPerLevel = 10  -- Number of variables per level

    -- Generate variable names format x$level$ L $index$
    varName :: Natural -> Natural -> String
    varName level idx = "x" ++ show level ++ "L" ++ show idx

    -- Define expressions for each variable
    genExpr :: Natural -> Natural -> Expr
    genExpr 1 idx = Nat idx
    genExpr level idx = Bin "+" (Var $ varName (level - 1) idx) (Nat idx)

    -- Generate DefTVar for each level
    genLevelDefs :: Natural -> [Definition]
    genLevelDefs 1 = [DefTVar (varName 1 idx) nat (genExpr 1 idx) | idx <- [1..varsPerLevel]]
    genLevelDefs level = genLevelDefs (level - 1) ++
        [DefTVar (varName level idx) nat (genExpr level idx) | idx <- [1..varsPerLevel]]

    --  sum of all xN_1 .. xN_10 + 100
    sumVars = foldl (\acc idx -> Bin "+" acc (Var $ varName n idx)) (Nat 100) [1..varsPerLevel]

    resultDef = DefTVar "result" nat sumVars

    in Module "DeepDependency_VariableModule" [ImportLib NatMod] $ trivial n decl
    , \n -> let -- 16 Description: Simple datatype declaration with a specified number of indices, defined implicitly.
        decl = [DefDataType "D" [("C1", Arr (Index (genIndex 'x' n) nat) (Con ("D" ++ " " ++ unwords (genIndex 'x' n))))] (Arr (genType n) Univ)]
        genType 1 = Con "Nat"
        genType m = Arr (genType (m-1)) nat

       in Module "DataImplicitIndices" [ImportLib NatMod] $ trivial n decl
    , \n -> let -- 17 Description: A file consisting of a single long line (length specified by the user).
        decl = [DefTVar "A" (Con "String") $ String (genLongValue n)]
        genLongValue 1 = "x"
        genLongValue m = 'x' : genLongValue (m-1)
        in Module "SingleLongLine" [ImportLib StringMod]  $ trivial n decl
    , \n ->  --18 Description: A single datatype where 'n' represents the number of 'Type' parameters, all needed for 'n' constructors
        let
            decl =  [DefPDataType "D" (map (\i -> ("p" ++ show i, Univ)) [1 .. n]) (map (\ i -> ("C" ++ show i,  PCon "D" (map (\j -> Con ("p" ++ show j) ) [1 .. n]))) [1 .. n]) Univ]
        in Module "ConstructorsParameters_Datatypes" [] $ trivial n decl
    , \n -> let -- 19  Description: A single datatype where 'n' represents the number of indices, all needed for 'n' constructors
        decl = [DefDataType "D"
           (map (\ i -> ("C" ++ show i, Arr (Index (genIndex 'x' i) nat) (PCon "D" ((map (\j -> Con ("X" ++ show j)) [1 .. i]) ++ map (\_ -> Con "0") [i+1..n])))) [1 .. n]) (Arr (genType n) Univ)]
        genType 1 = Con "Nat"
        genType m = Arr (genType (m-1)) nat

        in Module "IndicesConstructors_Datatypes" [ImportLib NatMod] $ trivial n decl
    , \n -> let -- 20  Description: A single datatype where 'n' represents the number of 'Type' parameters as well as the number of indices
        decl = [DefPDataType "D" (map (\i -> (nm 'p' i, Univ)) [1 .. n])
          [("C", Arr (Index (genIndex 'X' n) nat) (PCon "D" ((map (Con . nm 'p')  [1 .. n]) ++ map (Con . nm 'X') [1 .. n])))]
          (Arr (genType n) Univ)]
        genType 1 = Con "Nat"
        genType m = Arr (genType (m-1)) nat

        in Module "IndicesParameters_Datatypes" [ImportLib NatMod] $ trivial n decl
    ,  \n -> --21 Description: A function pattern matching on 'n' constructors of a datatype
        let
        decl = [DefDataType "D" (map (\ i -> (nm 'C' i, Con "D")) [1 .. n]) Univ, --create datatype
          OpenName "D",
          DefPatt "F" [("C", Con "D")] nat "C" (map (\i -> ([Arg (nm 'C' i) (Con "D")], Nat i)) [1..n]),
          DefTVar "N" nat (genCall n)]
        genCall p = foldr (\a b -> Bin "+" (FunCall "F" [Constructor (nm 'C' a)]) b) (FunCall "F" [Constructor "C1"]) 
          (reverse [2..p])
    in
       Module "Pattern_Matching_Datatypes" [ImportLib NatMod] $ trivial n decl
     ]


-- this is the list of expandable tests formatted as an IntMap so each test can be accessed by index
-- to access the expandable test at index i: tests ! i
tests :: IntMap (Natural -> Module)
tests = Map.fromList $ zip [1..(length _tests)] _tests
