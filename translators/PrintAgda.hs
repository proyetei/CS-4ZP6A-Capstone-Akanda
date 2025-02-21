{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module PrintAgda (runAgda) where

import Grammar

imports = "open import Agda.Builtin.IO  \nopen import Agda.Builtin.Nat \nopen import Data.Vec\n" --open import IO alone not scoping to stlib
datatype = "Set"

-- Print types
printType (Con t) = t
printType (Arr t1 t2) = printType t1 ++ " -> " ++ printType t2
printType (TVar t) = t
printType (PCon name types) = name ++ " " ++ unwords (map printType types)
-- Ensure `suc` is printed with parentheses
printType (DCon name types exprs) = -- For dependent type constructors (like suc)
    name ++ " " ++ unwords (map printType types) ++ " " ++ unwords (map printExpr exprs)
printType (Suc t) = "(suc " ++ printType t ++ ")"
-- Print expressions
printExpr (Var var) = var
printExpr (Int int) = show int
printExpr (Bool bool) = show bool
printExpr (String str) = str
printExpr (Mon op e) = "(" ++ op ++ printExpr e ++ ")"
printExpr (Bin op e1 e2) = printExpr e1 ++ " " ++ op ++ " " ++ printExpr e2
printExpr (Let [] expr) = printExpr expr -- this should never happen
printExpr (Let (d:[]) expr) = "let\n    " ++ printDef d ++ " in\n    " ++ printExpr expr
printExpr (Let (d:ds) expr) = "let\n    " ++ printDef d ++ concatMap (\x -> "\n    " ++ printDef x) ds ++ "\n    in " ++ printExpr expr -- Changed foldr to concatMap to preserve order
printExpr (If cond thn els) = "if " ++ printExpr cond ++ " then " ++ printExpr thn ++ " else " ++ printExpr els
printExpr (Where expr ds) = (++) (printExpr expr) $ (++) "\n    where " $ concatMap (\x -> "\n    " ++ printDef x) ds
printExpr (FunCall fun args) = fun ++ " " ++ unwords (map printExpr args) -- Added case for FunCall

-- For vectors
printExpr (VecEmpty) = "[]"
printExpr (VecCons expr rest) = printExpr expr ++ " ∷ " ++ printExpr rest


-- Function to print variable definitions
printDef (DefVar var ty expr) = typeSig ++ var ++ " = " ++ printExpr expr
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

-- Function to print datatype definitions
printDef (DefDataType name cons ty) = "data " ++ name ++ " : " ++ datatype ++ " where" ++ unwords (map (\(name, t) -> "\n " ++ name ++ " : " ++ printType t) cons) ++ "\n"



-- Function for records
printDef (DefRecType name maybeConName fields _) =
    "record " ++ name ++ " : Set where\n    constructor " ++ consName ++ "\n    field\n" ++
    concatMap (\(fname, ftype) -> "        " ++ fname ++ " : " ++ printType ftype ++ "\n") fields
  where
    consName = case maybeConName of
        Just conName -> conName
        Nothing -> "Const"  -- Default constructor name if not provided

printDef (InitRec name recType fields) =
    "\n" ++ name ++ " : " ++ recType ++ "\n" ++ 
    name ++ " = " ++ consName ++ concatMap (\(_, value) -> " (" ++ printExpr value ++ ")") fields
  where
    consName = case lookupConstructor recType of
        Just conName -> conName  -- Constructor Provided
        Nothing -> "Const"  -- Default to `Const` if no constructor is provided

    lookupConstructor :: String -> Maybe String
    lookupConstructor recType =
        case [c | DefRecType rName (Just c) _ _ <- definedRecords, rName == recType] of
            (c:_) -> Just c
            _ -> Nothing

-- Store all defined records to check constructors
definedRecords :: [Definition]
definedRecords = []



-- Print the Agda module
printAgda :: Module -> String
printAgda (Module name defs) =
    let
        headers = "module " ++ name ++ " where \n" ++ imports
        -- Concatenate all definitions
        body = concatMap printDef defs  -- Changed foldr to concatMap to preserve order

    in headers ++ "\n" ++ body

printAgda (File _ str) = str

runAgda :: Module -> IO()
runAgda m = do
    writeFile ("out/" ++ name ++ ".agda") $ printAgda m
        where 
            name = case m of 
                Module n _ -> n
                File n _ -> n
