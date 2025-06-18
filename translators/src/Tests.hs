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
genIndex c m = iter m (nm c)

v :: Char -> Natural -> Expr
v c n = Var $ nm c n

vx :: Natural -> Expr
vx = v 'x'

vxs :: Natural -> Expr
vxs n | n == 0    = Nat 1
      | otherwise = vx n

sum2vars :: Natural -> Expr
sum2vars n | n == 0    = Nat 1
           | otherwise = Bin Plus (vx n) (vx n)

nary :: Type -> Natural -> Type
nary t n = foldr Arr t (replicate (fromIntegral n) t)

-- this is our list of expandable tests. each test should take a Natural as an
-- argument and return a program written in the internal grammar (see
-- Grammar.hs)
_tests :: [Natural -> Module] -- todo: add commented Haskell representation for each test
_tests =
    [ \n -> let --1
            lets p = foldr (\a b -> Let [DefUVar (nm 'x' a) $ vxs (a-1)] b) (vxs p) [1..p]
        in Module "LetExample" [ImportLib NatMod] $ trivial n [DefTVar "n" nat $ lets n]

    , \n -> let --2
        lets p = foldr (\a b -> Let [DefUVar (nm 'x' a) $ sum2vars $ a-1] b) (vx p) [1..p]
    in Module "LetAddExample" [ImportLib NatMod] $ trivial n [DefTVar "n" nat $ lets n]

    , -- 3 Description: Generate Nested Functions
    \n -> let --3
            decl = [ DefTVar "n" nat (Let (reverse $ genFunc n) (genCall n)) ]
            -- Generate function definitions dynamically based on (1 to n)
            genFunc :: Natural -> [Definition]
            genFunc p = foldr (\a b -> DefNesFun (nm 'f' a) 
                                       (Just $ nary nat a)
                                       (iter a (\i -> Arg (nm 'x' i) nat))
                                       (foldl (\acc i -> Bin Plus acc (vx i)) (Nat 1) [1..a]) : b)
                              []
                              $ reverse [1..p]

            -- Generate function call expressions
            genCall :: Natural -> Expr
            genCall p = foldr (\a b -> Bin Plus (FunCall (nm 'f' a) (iter a (Nat . (+ 1)))) b)
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
        genSize p = Embed $ foldr (\_ b -> Suc b) (Var "f1") [2..p]

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
        -- Generate Record Definitions
        genRecords :: Natural -> [Definition]
        genRecords p = foldr (\(i,c) b -> DefRecType ("Record" ++ show i) [] ("Const" ++ show i) 
          [(nm 'f' i, c)] Univ : b) [] $ ((1,Con "Nat") : map (\i -> (i, Con ("Record" ++ show (i-1)))) [2..p])

        -- Generate Example Init
        genExample :: Natural -> Expr
        genExample p = foldr (\a b -> Paren $ (FunCall ("Const" ++ show a) [b])) (Nat 10) $ reverse [1..p]

        exampleInit = DefRec "example" (Con $ "Record" ++ show n) ("Const" ++ show n) [("example", genExample $ minusNatural n 1)] -- HACK
        decl = (genRecords n ++ [exampleInit])

        in Module "ChainDef_DependentRecordModule" [ImportLib NatMod] $ trivial n decl

    , -- 8 Description: Generate record with N parameters
    \n -> let
        decl = [recDef, exampleInit]
        -- Helper to build the sum exp 1 + 2 + ... + n
        buildSum m = foldr (\a b -> Bin Plus b (Nat a)) (Nat 1) $ reverse [2..m]

        -- Create param as a list of Args: f1 : Nat, f2 : Nat, …, fn : Nat
        params = iter n (\i -> Arg (nm 'f' i) nat)

        -- Define the record X with param, a constructor "Const",
        -- two fields "sums" and "values", and overall type Set.
        recDef = DefRecType "X" params "Const" [("sums", Con "Nat")] (Con "Set")

        -- Build the record type application as a string: "X 1 2 ... n"
        -- recTypeInstance = "X " ++ unwords (map show [1..n])
        recTypeInstance = DCon "X" [] $ iter n Nat

        -- Define the record instance "example" with computed field values:
        exampleInit = DefRec "example" recTypeInstance "Const" [("sums", Paren $ buildSum n)]
      in Module "Parameters_DependentRecordModule" [ImportLib NatMod] $ trivial n decl
    , \n -> -- 9
        -- Generate a file with n newlines where n = user input
        Module "NewlineFile" [] $ [Separator '\n' n False]

    , \n -> let -- 10 Description: A record declaration with N independent fields
        -- Generate field definitions dynamically
        genFields p = foldr (\a b -> (nm 'f' a, nat) : b) [] [1..p]
        -- Define the record structure
        xDef = DefRecType "Cap_X" [] "Const" (genFields n) Univ
        -- Generate example initialization dynamically
        genExample p = foldr (\a b -> (nm 'f' a, Nat 1) : b) [] [1..p]
        -- Define the example initialization
        exampleInit = DefRec "example" (Con "Cap_X") "Const" (genExample n)
    in Module "Fields_NonDependentRecordModule" [ImportLib NatMod] $ trivial n [xDef,exampleInit]

    , \n -> let -- 11 Description: Generate a very long chain (N) of independent record definitions
        exampleInit = DefRec "example" (Con $ "Record" ++ show n) ("Const" ++ show n) [("f1", Nat 1)]
        -- Generate Record Definitions
        genRecords :: Natural -> [Definition]
        genRecords p = foldl (\b a -> DefRecType ("Record" ++ show a) [] ("Const" ++ show a) [(nm 'f' a, nat)] Univ : b) 
                             [exampleInit] $ reverse [1..p]
    in Module "ChainDefFields_NonDependentRecordModule" [ImportLib NatMod] $ trivial n (genRecords n)

    , \n -> --12 Description: create a simple datatype with N constructors accepting no parameters
        Module "Constructors_Datatypes" [] $ trivial n [DefDataType "D" (iter n (\ i -> (nm 'C' i, Con "D"))) Univ]

    , \n ->  --13 Description: creates a datatype with a single constructor accepting N parameters
        let
            decl = [DefPDataType "D" (iter n (\i -> (nm 'p' i, Univ))) [("C", PCon "D" (iter n (Con . nm 'p')))] Univ]
        in Module "Parameters_Datatypes" [] $ trivial n decl

    , --14 Description: defines N variables, and uses both the first and last one in a declaration, N>=2
     \n ->
    let
        -- result = x1 + xn
        resultDef = DefTVar "result" nat $ Bin Plus (Var "x1") (vx n)

        decl = foldl (\b a -> DefTVar (nm 'x' a) nat (Nat a) : b) [resultDef] $ reverse [1..n]
    in Module "FirstLast_VariableModule" [ImportLib NatMod] $ trivial n decl
    , -- 15 Description: defines lots of dependent variables (10 at each level of dependency) and then use the most nested ones in a declaration
    \n -> let
    varsPerLevel = 10  -- Number of variables per level

    -- Generate variable names format x$level$ L $index$
    varName :: Natural -> Natural -> String
    varName level idx = nm 'x' level ++ nm 'L' idx

    -- Define expressions for each variable
    genExpr :: Natural -> Natural -> Expr
    genExpr idx level = if level == 0 then Nat idx else Bin Plus (Var $ varName level idx) (Nat idx)

    --  sum of all xN_1 .. xN_10 + 100
    resultDef = DefTVar "result" nat $ foldl (\acc idx -> Bin Plus acc (Var $ varName n idx)) (Nat 100) [1..varsPerLevel]

    -- Generate DefTVar for each level
    genLevelDefs :: Natural -> [Definition]
    genLevelDefs level = 
        foldl (\b a -> iter varsPerLevel (\idx -> DefTVar (varName a idx) nat (genExpr idx (a-1))) ++ b) [resultDef]
        $ reverse [1..level]

    in Module "DeepDependency_VariableModule" [ImportLib NatMod] $ trivial n (genLevelDefs n)

    , \n -> let -- 16 Description: Simple datatype declaration with a specified number of indices, defined implicitly.
        decl = [DefDataType "D" [("C1", Arr (Index (genIndex 'x' n) nat) (Con ("D " ++ unwords (genIndex 'x' n))))] 
                            (Arr (nary nat (n-1)) Univ)]
       in Module "DataImplicitIndices" [ImportLib NatMod] $ trivial n decl

    , \n -> let -- 17 Description: A file consisting of a single long line (length specified by the user).
        decl = [DefTVar "A" (Con "String") $ String $ replicate (fromIntegral n) 'x']
        in Module "SingleLongLine" [ImportLib StringMod]  $ trivial n decl

    , \n ->  --18 Description: A single datatype where 'n' represents the number of 'Type' parameters, all needed for 'n' constructors
        let
            decl =  [DefPDataType "D" (iter n (\i -> (nm 'p' i, Univ))) 
                                 (iter n (\ i -> (nm 'C' i,  PCon "D" (iter n (\j -> Con (nm 'p' j) )))) ) Univ]
        in Module "ConstructorsParameters_Datatypes" [] $ trivial n decl

    , \n -> let -- 19  Description: A single datatype where 'n' represents the number of indices, all needed for 'n' constructors
        decl = [DefDataType "D"
           (iter n (\ i -> (nm 'C' i, Arr (Index (genIndex 'x' i) nat)
                                          (PCon "D" $ iter n (\j -> if j <= i then Con (nm 'X' j) else Con "0"))
                                      ))) (Arr (nary nat (n-1)) Univ)]
        in Module "IndicesConstructors_Datatypes" [ImportLib NatMod] $ trivial n decl
    , \n -> let -- 20  Description: A single datatype where 'n' represents the number of 'Type' parameters as well as the number of indices
        decl = [DefPDataType "D" (iter n (\i -> (nm 'p' i, Univ)))
          [("C", Arr (Index (genIndex 'X' n) nat) (PCon "D" ((iter n (Con . nm 'p')) ++ iter n (Con . nm 'X'))))]
          (Arr (nary nat (n-1)) Univ)]
        in Module "IndicesParameters_Datatypes" [ImportLib NatMod] $ trivial n decl
    ,  \n -> --21 Description: A function pattern matching on 'n' constructors of a datatype
        let
        decl = [DefDataType "D" (iter n (\ i -> (nm 'C' i, Con "D"))) Univ, --create datatype
          OpenName "D",
          DefPatt "F" [("C", Con "D")] nat "C" (iter n (\i -> ([Arg (nm 'C' i) (Con "D")], Nat i))),
          DefTVar "N" nat (genCall n)]
        genCall p = foldr (\a b -> Bin Plus (FunCall "F" [Constructor (nm 'C' a)]) b) (FunCall "F" [Constructor "C1"]) 
          (reverse [2..p])
    in
       Module "Pattern_Matching_Datatypes" [ImportLib NatMod] $ trivial n decl
     ]


-- this is the list of expandable tests formatted as an IntMap so each test can be accessed by index
-- to access the expandable test at index i: tests ! i
tests :: IntMap (Natural -> Module)
tests = Map.fromList $ zip [1..(length _tests)] _tests
