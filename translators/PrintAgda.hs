{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module PrintAgda (runAgda) where

import Grammar

printImport (ImportLib "Nat") = "open import Agda.Builtin.Nat"
printImport (ImportLib "Vec") = "open import Data.Vec"
printImport (ImportLib "List") = "open import Agda.Builtin.List"
printImport (ImportLib "String") = "open import Agda.Builtin.String"
printImport (ImportLib _) = ""
printImport (ImportFun name lib) = "open import " ++ lib ++ " using (" ++ name ++ ")"

-- Print types
printType (Con "Type") = "Set"
printType (Con t) = t
printType (Arr t1 t2) = printType t1 ++ " -> " ++ printType t2
printType (TVar t) = t
printType (PCon name types) = name ++ " " ++ unwords (map printType types)
-- Ensure `suc` is printed with parentheses
printType (DCon name [] exprs) = -- For dependent type constructors (like suc)
    name ++ " " ++ unwords (map printExpr exprs)
printType (DCon name types exprs) = -- For dependent type constructors (like suc)
    name ++ " " ++ unwords (map printType types) ++ " " ++ unwords (map printExpr exprs)
printType (Suc t) = "(suc " ++ printType t ++ ")"
printType (Index names ty) = "{" ++ unwords names ++ " : " ++ printType ty ++ "}"
-- Print expressions
printExpr (Constructor name) = name
printExpr (Var var) = var
printExpr (Int int) = show int
printExpr (Bool bool) = show bool
printExpr (String str) = "\"" ++ str ++ "\""
printExpr (Paren e) = "(" ++ printExpr e ++ ")"
printExpr (Mon op e) = "(" ++ op ++ printExpr e ++ ")"
printExpr (Bin op e1 e2) = printExpr e1 ++ " " ++ op ++ " " ++ printExpr e2
printExpr (Let [] expr) = printExpr expr -- this should never happen
printExpr (Let (d:[]) expr) = "let\n    " ++ printDef d ++ " in\n    " ++ printExpr expr
printExpr (Let (d:ds) expr) = "let\n    " ++ printDef d ++ concatMap (\x -> "\n    " ++ printDef x) ds ++ "\n    in " ++ printExpr expr -- Changed foldr to concatMap to preserve order
printExpr (If cond thn els) = "if " ++ printExpr cond ++ " then " ++ printExpr thn ++ " else " ++ printExpr els
printExpr (Where expr ds) = (++) (printExpr expr) $ (++) "\n    where " $ concatMap (\x -> "\n    " ++ printDef x) ds
printExpr (FunCall fun args) = fun ++ " " ++ unwords (map printExpr args) -- Added case for FunCall
printExpr (VecEmpty) = "([])" -- For vectors
printExpr (VecCons expr xs) = "(" ++ printVecElements (VecCons expr xs) ++ ")"
printExpr (ListEmpty) = "([])"
printExpr (ListCons expr xs) = "(" ++ printListElements (ListCons expr xs) ++ ")"
printVecElements VecEmpty = "[]"
printVecElements (VecCons x xs) = printExpr x ++ " ∷ " ++ printVecElements xs
printListElements  ListEmpty = "[]"
printListElements  (ListCons x xs) = printExpr x ++ " ∷ " ++ printListElements  xs
-- Function to print variable definitions
printDef (DefVar var ty expr) = typeSig ++ var ++ " = " ++ printExpr expr ++ "\n"
    where
        typeSig = case ty of
            Just t -> var ++ " : " ++ printType t ++ "\n"
            Nothing -> ""

-- Function to print function definitions
printDef (DefFun var ty args expr) = typeSig ++ var ++ " " ++ argsStr ++ " = " ++ printExpr expr
    where
        typeSig = case ty of
            Just t -> var ++ " : " ++ printType t ++ "\n    "
            Nothing -> ""
        argsStr = unwords $ map (\(Arg name _) -> name) args  -- Correctly handle argument names

printDef (DefNesFun var Nothing args expr) = printDef (DefFun var Nothing args expr)
printDef (DefNesFun var (Just t) args expr) = printDef (DefFun var (Just t) args expr)
--Name [(Name,Type)] Type [([Arg], Expr)]
printDef (DefPatt var params ty _ cons) =
    var ++ " : " ++ (printType (foldr (\x y -> Arr x y) ty (map snd params))) ++ unwords (map (\(a,e) -> "\n" ++ var ++ " " ++ (unwords $ map (\(Arg name _) -> name) a) ++ " = " ++ printExpr e) cons) ++ "\n"
-- Function to print datatype definitions
printDef (DefDataType name cons ty) = "data " ++ name ++ " : " ++ printType ty ++ " where" ++ unwords (map (\(name, t) -> "\n " ++ name ++ " : " ++ printType t) cons) ++ "\n"
printDef (DefPDataType name params cons ty) = "data " ++ name ++ " " ++ unwords (map (\(x, y) -> "(" ++ x ++ " : " ++ printType y ++ ") ") params) ++ " : " ++ printType ty ++ " where" ++ unwords (map (\(name, t) -> "\n " ++ name ++ " : " ++ printType t) cons) ++ "\n"
--printDef (DefPDataType name params cons ty) = "data " ++ name ++ " : " ++ foldr (\x y -> "(" ++ x ++ " : Set) -> " ++ y) datatype params ++ " where" ++ unwords (map (\(name, t) -> "\n " ++ name ++ " : " ++ unwords (map (\p -> p ++ " ->") params) ++ printType t) cons) ++ "\n"


-- Function for records
printDef (DefRecType name params consName fields _) =
    "record " ++ name ++ paramsStr ++ " : Set where\n    constructor " ++ consName ++ "\n    field\n" ++
    concatMap (\(fname, ftype) -> "        " ++ fname ++ " : " ++ printType ftype ++ "\n") fields
    where
        paramsStr = case params of
            [] -> ""
            _ -> " " ++ unwords (map (\(Arg name ty) -> "(" ++ name ++ " : " ++ printType ty ++ ")") params)

printDef (DefRec name recType consName fields) =
    "\n" ++ name ++ " : " ++ printType recType ++ "\n" ++ 
    name ++ " = " ++ consName ++ concatMap (\(_, value) -> " " ++ printExpr value) fields

printDef (OpenName _) = ""

-- Store all defined records to check constructors
definedRecords :: [Definition]
definedRecords = []



-- Print the Agda module
printAgda :: Module -> String
printAgda (Module name imports defs) =
    let
        headers = "module " ++ name ++ " where \n" ++ unlines (map printImport imports)
        -- Concatenate all definitions
        body = concatMap printDef defs  -- Changed foldr to concatMap to preserve order

    in headers ++ "\n" ++ body

printAgda (File name str) = "module " ++ name ++ " where \n" ++ str

runAgda :: Module -> IO()
runAgda m = do
    writeFile ("out/" ++ name ++ ".agda") $ printAgda m
        where 
            name = case m of 
                Module n _ _ -> n
                File n _ -> n
