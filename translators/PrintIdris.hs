{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module PrintIdris (runIdris) where

import Grammar

imports = "import Data.Vect"
datatype = "Type"

printType (Con t) = t
printType (Arr t1 t2) = printType t1 ++ " -> " ++ printType t2
printType (TVar t) = t
printType (PCon "Vec" [Con baseType, size]) = "Vect " ++ printType size ++ " " ++ baseType
printType (PCon name types) = name ++ " " ++ unwords (map printType types)
-- Ensure `suc` is printed with parentheses
printType (DCon name types exprs) = -- For dependent type constructors (like suc)
    name ++ " " ++ unwords (map printType types) ++ " " ++ unwords (map printExpr exprs)
printType (Suc t) = "(S " ++ printType t ++ ")"
printExpr (Var var) = var
printExpr (Int int) = show int
printExpr (Bool bool) = show bool
printExpr (String str) = str
printExpr (Mon op e) = "(" ++ op ++ printExpr e ++ ")"
printExpr (Bin op e1 e2) = printExpr e1 ++ " "  ++ op ++ " " ++ printExpr e2
printExpr (Let [] expr) = printExpr expr -- this should never happen
printExpr (Let (d:[]) expr) = "let " ++ (printDef d) ++ " in \n    " ++ printExpr expr
printExpr (Let (d:ds) expr) = "let \n    " ++ (foldl (\x y -> x ++ "\n    " ++ y) (printDef d) $ map printDef ds) ++ "\n    in \n    " ++ printExpr expr -- probably need to recursively indent blocks to make sure everything stays aligned
printExpr (If cond thn els) = "if " ++ printExpr cond ++ " then " ++ printExpr thn ++ " else " ++ printExpr els
printExpr (Where expr ds) = (++) (printExpr expr) $ (++) "\n    where " $ foldl (\x y -> x ++ "\n    " ++ y) "" $ map printDef ds
printExpr (FunCall fun args) = fun ++ " " ++ (unwords $ map printExpr args) -- Added case for FunCall
-- For vectors
printExpr VecEmpty = ""
printExpr (VecCons x VecEmpty) = "[" ++ printExpr x ++ "]"  -- Single-element list
printExpr (VecCons x xs) = "[" ++ printExpr x ++ concatMap (\e -> ", " ++ printExpr e) (formatVec xs) ++ "]"
  where
    -- Helper function to collect elements into a list
    formatVec :: Expr -> [Expr]
    formatVec VecEmpty = []
    formatVec (VecCons y ys) = y : formatVec ys
    formatVec other = [other]  -- In case of unexpected structures

printDef (DefVar var ty expr) = typeSig ++ var ++ " = " ++ printExpr expr where
    typeSig = case ty of
        Just t -> var ++ " : " ++ printType t ++ "\n"
        Nothing -> ""
printDef (DefFun var ty args expr) = typeSig ++ var ++ (foldl (\x y -> x ++ " " ++ y) "" $ map arg args) ++ " = " ++ printExpr expr where
    typeSig = case ty of
        Just t -> var ++ " : " ++ printType t ++ "\n    "
        Nothing -> ""
printDef (DefNesFun var Nothing args expr) = printDef (DefFun var Nothing args expr)
printDef (DefNesFun var (Just t) args expr) = printDef (DefFun var (Just t) args expr)
printDef (DefDataType name cons ty) = "data " ++ name ++ " : " ++ datatype ++ " where" ++ unwords (map (\(name, t) -> "\n " ++ name ++ " : " ++ printType t) cons) ++ "\n"

-- Function for records
printDef (DefRecType name maybeConName fields _) =
    "record " ++ name ++ " where\n    constructor " ++consName ++"\n" ++
    unlines (map (\(fname, ftype) -> "    " ++ fname ++ " : " ++ printType ftype) fields)
    where
        consName = case maybeConName of
            Just conName -> conName
            Nothing -> "Const"  -- Default constructor name if not provided

printDef (InitRec name recType fields) =
    name ++ " : " ++ recType ++ 
    "\n" ++ name ++ " = " ++ consName ++ concatMap (\(_, value) -> " " ++ printExpr value) fields ++ "\n"
  where
    -- Determine constructor name, defaulting to `Const`
    consName = case lookupConstructor recType of
        Just conName -> conName  -- ✅ Constructor Provided (e.g., `dynamicList`)
        Nothing -> "Const"  -- ✅ Default to `Const` if no constructor

    -- Lookup the constructor from defined records
    lookupConstructor :: String -> Maybe String
    lookupConstructor recType =
        case [c | DefRecType rName (Just c) _ _ <- definedRecords, rName == recType] of
            (c:_) -> Just c
            _ -> Nothing

-- Catch-all to prevent non-exhaustive errors
printDef _ = error "Unhandled case in printDef"

-- Store all defined records to check constructors
definedRecords :: [Definition]
definedRecords = []



printIdris :: Module -> String
printIdris (Module name defs) =
    let
        headers = "module Main\n" ++ imports
        body = foldl (\x y -> x ++ "\n" ++ y) "" $ map printDef defs
    in headers ++ "\n" ++ body ++ "\nmain : IO()\nmain = putStrLn \"\""

printIdris (File _ str) = str

runIdris :: Module -> IO()
runIdris m = do
    writeFile ("out/" ++ name ++ ".idr") $ printIdris m
        where
            name = case m of
                Module n _ -> n
                File n _ -> n