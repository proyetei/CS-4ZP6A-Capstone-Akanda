module PrintRocq (runRocq) where

import Grammar
import Data.Char
import Data.List (isPrefixOf)

printImport (ImportLib "Vec") = "Require Import Coq.Vectors.Vector. \nImport VectorNotations."
printImport (ImportLib "String") = "Require Import Coq.Strings.String."
printImport (ImportLib _) = ""
printImport (ImportFun name lib) = "Require Import " ++ lib ++ " using (" ++ name ++ ")\n"

printType (Con "Type") = "Type"
printType (Con t) = if  "Cap_" `isPrefixOf` t || 
                        "Record" `isPrefixOf` t 
                        then t else (map toLower t) -- if starts with keyword Cap_ maintain, else lower case
--printType (Con t) = map toLower t
printType (Arr t1 t2) = printType t1 ++ " -> " ++ printType t2
printType (TVar t) = t
printType (PCon "Vec" args) = "t " ++ unwords (map printType args)
printType (PCon name types) = (map toLower name) ++ " " ++ unwords (map printType types)
printType (DCon name [] exprs) = -- For dependent type constructors (like suc)
    name ++ " " ++ unwords (map printExpr exprs)
printType (DCon name types exprs) = name ++ " " ++ unwords (map printType types) ++ " " ++ unwords (map printExpr exprs)
printType (Suc t) = "(S " ++ printType t ++ ")" --
printType (Index names ty) = "forall {" ++ map toLower (unwords names) ++ " : " ++ printType ty ++ "}"
printReturnType (Con t) = map toLower t --required for nested functions
printReturnType (Arr _ t) = printReturnType t
printArg a = "(" ++ (arg a) ++ " : " ++ (printType $ argty a) ++ ")"
printExpr (Constructor name) = map toLower name
printExpr (Var var) = var
printExpr (Int int) = show int
printExpr (Bool bool) = show bool
printExpr (String str) = "\"" ++ str ++ "\""
printExpr (Paren e) = "(" ++ printExpr e ++ ")"
printExpr (Mon op e) = "(" ++ op ++ printExpr e ++ ")"
printExpr (Bin op e1 e2) = printExpr e1 ++ " " ++ op ++ " " ++ printExpr e2
printExpr (Let [] expr) = printExpr expr -- this should never happen
printExpr (Let (d:ds) expr) =
    foldl (\acc def -> acc ++ "let " ++ printDef def ++ " in\n    ") "" (d:ds) ++ printExpr expr --modified for proper let-in nesting
printExpr (If cond thn els) = "if " ++ printExpr cond ++ " then " ++ printExpr thn ++ " else " ++ printExpr els
printExpr (Where expr ds) = (++) (printExpr expr) $ (++) "\n\twhere " $ foldl (\x y -> x ++ "\n\t" ++ y) "" $ map printDef ds
printExpr (FunCall fun args) = fun ++ " " ++ (unwords $ map printExpr args) -- Added case for FunCall
-- For vectors and lists
printExpr VecEmpty = "[]"
printExpr (VecCons x xs) = "[" ++ printVecElements (VecCons x xs) ++ "]"
printExpr ListEmpty = "[]"
printExpr (ListCons x xs) = "[" ++ printListElements (ListCons x xs) ++ "]"

printVecElements VecEmpty = ""
printVecElements (VecCons x VecEmpty) = printExpr x
printVecElements (VecCons x xs) = printExpr x ++ "; " ++ printVecElements xs
printListElements ListEmpty = ""
printListElements (ListCons x ListEmpty) = printExpr x
printListElements (ListCons x xs) = printExpr x ++ "; " ++ printListElements xs
printDef (DefVar var Nothing expr) = var ++ " := " ++ printExpr expr
printDef (DefVar var (Just t) expr) = "Definition " ++ var ++ " : " ++ printType t ++ " := " ++ printExpr expr ++ ". \n"--LEAVING OUT: Compute " ++ var ++ "."
printDef (DefFun var Nothing args expr) = var ++ (foldl (\x y -> x ++ " " ++ y) "" $ map arg args) ++ " := " ++ printExpr expr
printDef (DefFun var (Just t) args expr) = "Definition " ++ var ++ (foldl (\x y -> x ++ " " ++ y) "" $ map printArg args) ++ " : " ++ printType t ++ " := " ++ printExpr expr ++ "."
printDef (DefNesFun var Nothing args expr) = var ++ " " ++ (unwords $ map arg args) ++ " := " ++ printExpr expr
printDef (DefNesFun var (Just t) args expr) = var ++ " " ++ (unwords $ map printArg args) ++ " : " ++ printReturnType t ++ " := " ++ printExpr expr

printDef (DefPatt var params ty m cons) = "Fixpoint " ++ var ++ " " ++ (unwords $ map (\(x, y) -> " (" ++ x ++ " : " ++ printType y ++ ")") params) ++ " : " ++ printType ty ++ " := \nmatch " ++ m ++ " with" ++ unwords (map (\(a, e) -> "\n| " ++ (unwords $ map (\(Arg name _) -> map toLower name) a) ++ " => " ++ printExpr e) cons) ++ " end."
printDef (DefDataType name args ty) = let
    printIndices :: Type -> String
    printIndices (Arr (Index n t) ctype) = printType(Index n t)  ++ ", " ++ printType ctype
    printIndices t = printType t
    in
        "Inductive " ++ map toLower name ++ " : " ++ printType ty ++ " := " ++ unwords (map (\(x, y) -> "\n| " ++ map toLower x ++ " : " ++ printIndices y) args) ++ "."
printDef (DefPDataType name params args ty) = let
    printIndices :: Type -> String
    printIndices (Arr (Index n t) ctype) = printType(Index n t)  ++ ", " ++ printType ctype
    printIndices t = printType t
    in
        "Inductive " ++ map toLower name ++ unwords (map (\(x, y) -> " (" ++ map toLower x ++ ": " ++ printType y ++ ")") params) ++ " : " ++ printType ty ++ " := " ++ unwords (map (\(x, y) -> "\n| " ++ map toLower x ++ " : " ++ printIndices y) args) ++ "."

--Function for Records
printDef (DefRecType name params consName fields _) =
    "Record " ++ name ++ paramsStr ++ " : Type := " ++ consName ++ " {\n" ++
    concatMap (\(fname, ftype) -> "  " ++ fname ++ " : " ++ printType ftype ++ ";\n") fields ++
    "}.\n"
    where
        paramsStr = case params of
            [] -> ""
            _ -> " " ++ unwords (map (\(Arg n t) -> "(" ++ n ++ " : " ++ printType t ++ ")") params)


printDef (DefRec name recType consName fields) =
  "Definition " ++ name ++ " : " ++ printType recType ++ " :=\n  " ++ constructorCall ++ ".\n"
  where
    -- Split the record type into words.
    -- The first word is the record name; any remaining words are the concrete parameter values.
    recWords = words $ printType recType
    baseName = if null recWords then printType recType else head recWords
    paramsStr = case recWords of
                  (_:ps) -> unwords ps
                  _      -> ""

    -- Use the provided constructor name if available; otherwise, look up the default.

    -- Build a string for the field values; each field value is preceded by a space.
    fieldsStr = concatMap (\(_, value) -> " " ++ printExpr value) fields

    -- If there are parameters, insert them between the constructor name and the field values.
    constructorCall = if null paramsStr
                      then consName ++ fieldsStr
                      else consName ++ " " ++ paramsStr ++ fieldsStr
printDef (OpenName _) = ""
printDef (DefModule m) = printModule m

-- Store all defined records to check constructors
definedRecords :: [Definition]
definedRecords = []


printModule :: Module -> String
printModule (Module name imports defs) =
    let
        headers = unlines (map printImport imports) ++ "\n\nModule " ++ name ++ ".\n" 
        body = foldl (\x y -> x ++ "\n" ++ y) "" $ map printDef defs
    in headers ++ "\n" ++ body ++ "\nEnd " ++ name ++ "."

printModule (File name str) = "Module " ++ name ++ ".\n\n" ++ str ++ "\nEnd " ++ name ++ "."

runRocq :: Module -> IO()
runRocq m = do
    writeFile ("out/" ++ name ++ ".v") $ printModule m
        where
            name = case m of
                Module n _ _ -> n
                File n _ -> n
