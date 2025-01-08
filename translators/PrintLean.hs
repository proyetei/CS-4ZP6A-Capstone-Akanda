module PrintLean (runLean) where

import Grammar

imports = ""

printLean :: Module -> String
printLean (Module name defs) =
    let
        headers = imports
        printType (Con t) = t
        printType (Arr t1 t2) = printType t1 ++ " -> " ++ printType t2
        printArg a = "(" ++ (arg a) ++ " : " ++ (printType $ ty a) ++ ")"
        printExpr (Var var) = var
        printExpr (Int int) = show int
        printExpr (Bool bool) = show bool
        printExpr (String str) = str
        printExpr (Mon op e) = "(" ++ op ++ printExpr e ++ ")"
        printExpr (Bin op e1 e2) = printExpr e1 ++ op ++ printExpr e2
        printExpr (Let [] expr) = printExpr expr -- this should never happen
        printExpr (Let (d:[]) expr) = "let " ++ (printDef d) ++ "\n" ++ printExpr expr
        printExpr (Let (d:ds) expr) = "let " ++ (foldr (\x y -> x ++ "\nlet " ++ y) (printDef d) $ map printDef ds) ++ "\n" ++ printExpr expr -- probably need to recursively indent blocks to make sure everything stays aligned
        printExpr (If cond thn els) = "if " ++ printExpr cond ++ " then \n\t" ++ printExpr thn ++ "\nelse " ++ printExpr els
        printExpr (Where expr ds) = (++) (printExpr expr) $ (++) "\n\twhere " $ foldr (\x y -> x ++ "\n\t" ++ y) "" $ map printDef ds
        printExpr (FunCall fun args) = fun ++ " " ++ (unwords $ map printExpr args) -- Added case for FunCall
        printDef (DefVar var Nothing expr) = var ++ " := " ++ printExpr expr 
        printDef (DefVar var (Just t) expr) = "def " ++ var ++ " : " ++ printType t ++ " := " ++ printExpr expr
        printDef (DefFun var Nothing args expr) = var ++ (foldr (\x y -> x ++ " " ++ y) "" $ map arg args) ++ " := " ++ printExpr expr 
        printDef (DefFun var (Just t) args expr) = "def " ++ var ++ (foldr (\x y -> x ++ " " ++ y) "" $ map printArg args) ++ " : " ++ printType t ++ " := " ++ printExpr expr
        body = foldr (\x y -> x ++ "\n" ++ y) "" $ map printDef defs
    in headers ++ "\n" ++ body

runLean :: Module -> IO()
runLean (Module name d) = do
    writeFile ("out/" ++ name ++ ".lean") $ printLean (Module name d)