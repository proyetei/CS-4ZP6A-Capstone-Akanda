module PrintRocq (runRocq) where

import Grammar
    ( Arg(arg, ty),
      Definition(DefNesFun, DefVar, DefFun, DefRecType, InitRec, DefDataType),
      Expr(FunCall, Var, Int, Bool, String, Mon, Bin, Let, If, Where, VecEmpty, VecCons),
      Module(File, Module),
      Type(Arr, Con, TVar, PCon, DCon, Suc) )
import Data.Char

imports = "Require Import Coq.Vectors.Vector. \nImport VectorNotations."
datatype = "Type"

printType (Con t) = map toLower t
printType (Arr t1 t2) = printType t1 ++ " -> " ++ printType t2
printType (TVar t) = t
printType (PCon "Vec" args) = "t " ++ unwords (map printType args)
printType (PCon name types) = name ++ " " ++ unwords (map printType types)
printType (DCon name types exprs) = name ++ " " ++ unwords (map printType types) ++ " " ++ unwords (map printExpr exprs)
printType (Suc t) = "(S " ++ printType t ++ ")" -- Use `Nat.succ` explicitly
printReturnType (Con t) = map toLower t --required for nested functions
printReturnType (Arr _ t) = printReturnType t
printArg a = "(" ++ (arg a) ++ " : " ++ (printType $ ty a) ++ ")"
printExpr (Var var) = var
printExpr (Int int) = show int
printExpr (Bool bool) = show bool
printExpr (String str) = str
printExpr (Mon op e) = "(" ++ op ++ printExpr e ++ ")"
printExpr (Bin op e1 e2) = printExpr e1 ++ " " ++ op ++ " " ++ printExpr e2
printExpr (Let [] expr) = printExpr expr -- this should never happen
printExpr (Let (d:ds) expr) =
    foldl (\acc def -> acc ++ "let " ++ printDef def ++ " in\n    ") "" (d:ds) ++ printExpr expr --modified for proper let-in nesting
printExpr (If cond thn els) = "if " ++ printExpr cond ++ " then " ++ printExpr thn ++ " else " ++ printExpr els
printExpr (Where expr ds) = (++) (printExpr expr) $ (++) "\n\twhere " $ foldl (\x y -> x ++ "\n\t" ++ y) "" $ map printDef ds
printExpr (FunCall fun args) = fun ++ " " ++ (unwords $ map printExpr args) -- Added case for FunCall

--For vector
printExpr VecEmpty = "[]"  -- Empty Vector in Coq
printExpr (VecCons expr VecEmpty) = "[" ++ printExpr expr ++ "]"  -- Single-element Vector
printExpr (VecCons expr rest) = "[" ++ printVecElements (VecCons expr rest) ++ "]"

printVecElements :: Expr -> String
printVecElements VecEmpty = ""
printVecElements (VecCons x VecEmpty) = printExpr x
printVecElements (VecCons x xs) = printExpr x ++ "; " ++ printVecElements xs


printDef (DefVar var Nothing expr) = var ++ " := " ++ printExpr expr
printDef (DefVar var (Just t) expr) = "Definition " ++ var ++ " : " ++ printType t ++ " := " ++ printExpr expr ++ ". \nCompute " ++ var ++ "."
printDef (DefFun var Nothing args expr) = var ++ (foldl (\x y -> x ++ " " ++ y) "" $ map arg args) ++ " := " ++ printExpr expr
printDef (DefFun var (Just t) args expr) = "Definition " ++ var ++ (foldl (\x y -> x ++ " " ++ y) "" $ map printArg args) ++ " : " ++ printType t ++ " := " ++ printExpr expr ++ "."
printDef (DefNesFun var Nothing args expr) =
    var ++ " " ++ (unwords $ map arg args) ++ " := " ++ printExpr expr
printDef (DefNesFun var (Just t) args expr) =
    var ++ " " ++ (unwords $ map printArg args) ++ " : " ++ printReturnType t ++ " := " ++ printExpr expr
printDef (DefDataType name args ty) = "Inductive " ++ name ++ " : " ++ datatype ++ " := " ++ unwords (map (\(x, y) -> "\n| " ++ x ++ " : " ++ (printType y)) args)


--Function for Records
printDef (DefRecType name maybeConName fields _) =
    "Record " ++ name ++ " : Type := " ++ constructorName ++ " {\n" ++
    concatMap (\(fname, ftype) -> "  " ++ fname ++ " : " ++ printType ftype ++ ";\n") fields ++
    "}.\n"
  where
    constructorName = case maybeConName of
        Just conName -> conName  -- ✅ Use provided constructor
        Nothing -> "Const"  -- ✅ Default to `"Const"` if no constructor is provided

printDef (InitRec name recType fields) =
    "Definition " ++ name ++ " : " ++ recType ++ " :=\n  " ++ constructorName ++ " " ++
    concatMap printField fields ++ ".\n"
  where
    printField (fname, value) = "\n    " ++ printExpr value
    constructorName = case lookupConstructor recType of
        Just conName -> conName  -- Use provided constructor 
        Nothing -> "Const"  -- Default to `"Const"` if no constructor

    -- Function to find the constructor from defined records
    lookupConstructor :: String -> Maybe String
    lookupConstructor recType =
        case [c | DefRecType rName (Just c) _ _ <- definedRecords, rName == recType] of
            (c:_) -> Just c
            _ -> Nothing

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