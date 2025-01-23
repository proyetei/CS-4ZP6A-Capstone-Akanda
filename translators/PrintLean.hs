module PrintLean (runLean) where

import Grammar

imports = ""

printType (Con t) = t
printType (Arr t1 t2) = printType t1 ++ " -> " ++ printType t2
printReturnType (Con t) = t --required for nested functions
printReturnType (Arr _ t) = printReturnType t
printArg a = "(" ++ (arg a) ++ " : " ++ (printType $ ty a) ++ ")"
printExpr (Var var) = var
printExpr (Int int) = show int
printExpr (Bool bool) = show bool
printExpr (String str) = str
printExpr (Mon op e) = "(" ++ op ++ printExpr e ++ ")"
printExpr (Bin op e1 e2) = printExpr e1 ++ " " ++ op ++ " " ++ printExpr e2
printExpr (Let [] expr) = printExpr expr -- this should never happen
printExpr (Let (d:[]) expr) = "let " ++ (printDef d) ++ "\n" ++ printExpr expr
printExpr (Let (d:ds) expr) = "let " ++ (foldl (\x y -> x ++ "\nlet " ++ y) (printDef d) $ map printDef ds) ++ "\n" ++ printExpr expr -- probably need to recursively indent blocks to make sure everything stays aligned
printExpr (If cond thn els) = "if " ++ printExpr cond ++ " then \n\t" ++ printExpr thn ++ "\nelse " ++ printExpr els
printExpr (Where expr ds) = (++) (printExpr expr) $ (++) "\n\twhere " $ foldl (\x y -> x ++ "\n\t" ++ y) "" $ map printDef ds
printExpr (FunCall fun args) = fun ++ " " ++ (unwords $ map printExpr args) -- Added case for FunCall
printDef (DefVar var Nothing expr) = var ++ " := \n" ++ printExpr expr 
printDef (DefVar var (Just t) expr) = "def " ++ var ++ " : " ++ printType t ++ " := \n" ++ printExpr expr
printDef (DefFun var Nothing args expr) = var ++ (foldl (\x y -> x ++ " " ++ y) "" $ map arg args) ++ " := " ++ printExpr expr 
printDef (DefFun var (Just t) args expr) = "def " ++ var ++ (foldl (\x y -> x ++ " " ++ y) "" $ map printArg args) ++ " : " ++ printType t ++ " := " ++ printExpr expr
printDef (DefNesFun var Nothing args expr) =
    var ++ " " ++ (unwords $ map arg args) ++ " := " ++ printExpr expr
printDef (DefNesFun var (Just t) args expr) =
    var ++ " " ++ (unwords $ map printArg args) ++ " : " ++ printReturnType t ++ " := " ++ printExpr expr

printLean :: Module -> String
printLean (Module name defs) =
    let
        headers = imports
        body = foldl (\x y -> x ++ "\n" ++ y) "" $ map printDef defs
    in headers ++ "\n" ++ body

printLean (File _ str) = str

runLean :: Module -> IO()
runLean m = do
    writeFile ("out/" ++ name ++ ".lean") $ printLean m
        where 
            name = case m of 
                Module n _ -> n
                File n _ -> n