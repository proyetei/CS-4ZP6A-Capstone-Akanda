module PrintRocq (runRocq) where

import Grammar
    ( Arg(arg, ty, Arg),
      Definition(DefNesFun, DefVar, DefFun, DefRecType, InitRec, DefDataType, DefPDataType, DefPatt, OpenName),
      Expr(FunCall, Var, Int, Bool, String, Mon, Bin, Let, If, Where, VecEmpty, VecCons, Paren, ListEmpty, ListCons),
      Module(File, Module),
      Type(Arr, Con, TVar, PCon, DCon, Suc, Index) )
import Data.Char
import Data.List (isPrefixOf)

imports = "Require Import Coq.Vectors.Vector. \nRequire Import List. \nImport VectorNotations. \nImport ListNotations.\n"
datatype = "Type"

printType (Con "Type") = "Type"
printType (Con t) =
  (if "Record" `isPrefixOf` t then t else (map toLower t)) --Rec type should stay cap
--printType (Con t) = map toLower t
printType (Arr t1 t2) = printType t1 ++ " -> " ++ printType t2
printType (TVar t) = t
printType (PCon "Vec" args) = "t " ++ unwords (map printType args)
printType (PCon name types) = (map toLower name) ++ " " ++ unwords (map printType types)
printType (DCon name types exprs) = name ++ " " ++ unwords (map printType types) ++ " " ++ unwords (map printExpr exprs)
printType (Suc t) = "(S " ++ printType t ++ ")" --
printType (Index names ty) = "forall {" ++ unwords names ++ " : " ++ printType ty ++ "}"
printReturnType (Con t) = map toLower t --required for nested functions
printReturnType (Arr _ t) = printReturnType t
printArg a = "(" ++ (arg a) ++ " : " ++ (printType $ ty a) ++ ")"
printExpr (Var var) = var
printExpr (Int int) = show int
printExpr (Bool bool) = show bool
printExpr (String str) = str
printExpr (Paren e) = "(" ++ printExpr e ++ ") "
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

printDef (DefPatt var params ty m cons) = var ++ " " ++ (unwords $ map (\(x, y) -> " (" ++ x ++ " : " ++ printType y ++ ")") params) ++ " : " ++ printType ty ++ " := \nmatch " ++ m ++ " with\n" ++ unwords (map (\(a, e) -> "\n| " ++ (unwords $ map printArg a) ++ " => " ++ printExpr e) cons) ++ "."
printDef (DefDataType name args ty) = "Inductive " ++ map toLower name ++ " : " ++ printType ty ++ " := " ++ unwords (map (\(x, y) -> "\n| " ++ map toLower x ++ " : " ++ (printType y)) args) ++ "."
printDef (DefPDataType name params args ty) = "Inductive " ++ map toLower name ++ unwords (map (\x -> " (" ++ map toLower x ++ ": Type)") params) ++ " : " ++ printType ty ++ " := " ++ unwords (map (\(x, y) -> "\n| " ++ map toLower x ++ " : " ++ (printType y)) args) ++ "."

--Function for Records
printDef (DefRecType name params maybeConName fields _) =
    "Record " ++ name ++ paramsStr ++ " : Type := " ++ consName ++ " {\n" ++
    concatMap (\(fname, ftype) -> "  " ++ fname ++ " : " ++ printType ftype ++ ";\n") fields ++
    "}.\n"
    where
        consName = case maybeConName of
            Just conName -> conName  -- Use provided constructor
            Nothing      -> "Const"   -- Default to "Const" if none provided
        paramsStr = case params of
            Just args -> " " ++ unwords (map (\(Arg n t) -> "(" ++ n ++ " : " ++ printType t ++ ")") args)
            Nothing   -> ""


printDef (InitRec name recType maybeConsName fields) =
  "Definition " ++ name ++ " : " ++ recType ++ " :=\n  " ++ constructorCall ++ ".\n"
  where
    -- Split the record type into words.
    -- The first word is the record name; any remaining words are the concrete parameter values.
    recWords = words recType
    baseName = if null recWords then recType else head recWords
    paramsStr = case recWords of
                  (_:ps) -> unwords ps
                  _      -> ""

    -- Use the provided constructor name if available; otherwise, look up the default.
    consName = case maybeConsName of
                 Just c -> c
                 Nothing -> case lookupConstructor baseName of
                              Just con -> con
                              Nothing -> "Const"  -- Default if no constructor found

    -- Build a string for the field values; each field value is preceded by a space.
    fieldsStr = concatMap (\(_, value) -> " " ++ printExpr value) fields

    -- If there are parameters, insert them between the constructor name and the field values.
    constructorCall = if null paramsStr
                      then consName ++ fieldsStr
                      else consName ++ " " ++ paramsStr ++ fieldsStr

    -- Function to find the constructor from defined records
    lookupConstructor :: String -> Maybe String
    lookupConstructor recType =
      case [c | DefRecType rName _ (Just c) _ _ <- definedRecords, rName == recType] of
        (c:_) -> Just c
        _ -> Nothing
printDef (OpenName _) = ""

-- Store all defined records to check constructors
definedRecords :: [Definition]
definedRecords = []


printRocq :: Module -> String
printRocq (Module name defs) =
    let
        headers = imports ++ "\n\nModule " ++ name ++ ".\n" 
        body = foldl (\x y -> x ++ "\n" ++ y) "" $ map printDef defs
    in headers ++ "\n" ++ body ++ "\nEnd " ++ name ++ "."

printRocq (File _ str) = str

runRocq :: Module -> IO()
runRocq m = do
    writeFile ("out/" ++ name ++ ".v") $ printRocq m
        where
            name = case m of
                Module n _ -> n
                File n _ -> n