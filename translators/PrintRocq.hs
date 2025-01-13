module PrintRocq (runRocq) where

import Grammar
import Data.Char

imports = ""

printRocq :: Module -> String
printRocq (Module name defs) =
    let
        headers = "Module " ++ name ++ ".\n" ++ imports
        printType (Con t) = map toLower t
        printType (Arr t1 t2) = printType t1 ++ " -> " ++ printType t2
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
        printDef (DefVar var Nothing expr) = var ++ " := " ++ printExpr expr 
        printDef (DefVar var (Just t) expr) = "Definition " ++ var ++ " : " ++ printType t ++ " := " ++ printExpr expr ++ ". \nCompute " ++ var ++ "."
        printDef (DefFun var Nothing args expr) = var ++ (foldl (\x y -> x ++ " " ++ y) "" $ map arg args) ++ " := " ++ printExpr expr 
        printDef (DefFun var (Just t) args expr) = "Definition " ++ var ++ (foldl (\x y -> x ++ " " ++ y) "" $ map printArg args) ++ " : " ++ printType t ++ " := " ++ printExpr expr ++ "."
        printDef (DefNesFun var Nothing args expr) =
            var ++ " " ++ (unwords $ map arg args) ++ " := " ++ printExpr expr
        printDef (DefNesFun var (Just t) args expr) =
            var ++ " " ++ (unwords $ map printArg args) ++ " : " ++ printReturnType t ++ " := " ++ printExpr expr
        body = foldl (\x y -> x ++ "\n" ++ y) "" $ map printDef defs
    in headers ++ "\n" ++ body ++ "\nEnd " ++ name ++ "."

runRocq :: Module -> IO()
runRocq (Module name d) = do
    writeFile ("out/" ++ name ++ ".v") $ printRocq (Module name d)