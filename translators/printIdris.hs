module PrintIdris (runIdris) where

import Grammar

imports = ""

printIdris :: Module -> String
printIdris (Module name defs) =
    let
        headers = "module " ++ name ++ "\n" ++ imports
        printType (Con t) = t
        printType (Arr t1 t2) = printType t1 ++ " -> " ++ printType t2
        printExpr (Var var) = var
        printExpr (Int int) = show int
        printExpr (Bool bool) = show bool
        printExpr (String str) = str
        printExpr (Mon op e) = "(" ++ op ++ printExpr e ++ ")"
        printExpr (Bin op e1 e2) = printExpr e1 ++ op ++ printExpr e2
        printExpr (Let [] expr) = printExpr expr -- this should never happen
        printExpr (Let (d:[]) expr) = "let " ++ (printDef d) ++ " in \n\t" ++ printExpr expr
        printExpr (Let (d:ds) expr) = "let \n\t" ++ (foldr (\x y -> x ++ "\n\t" ++ y) (printDef d) $ map printDef ds) ++ "\n\tin \n\t" ++ printExpr expr -- probably need to recursively indent blocks to make sure everything stays aligned
        printExpr (If cond thn els) = "if " ++ printExpr cond ++ " then " ++ printExpr thn ++ " else " ++ printExpr els
        printExpr (Where expr ds) = (++) (printExpr expr) $ (++) "\n\twhere " $ foldr (\x y -> x ++ "\n\t" ++ y) "" $ map printDef ds
        printDef (DefVar var ty expr) = typeSig ++ var ++ " = " ++ printExpr expr where
            typeSig = case ty of
                Just t -> var ++ " : " ++ printType t ++ "\n"
                Nothing -> ""
        printDef (DefFun var ty args expr) = typeSig ++ var ++ (foldr (\x y -> x ++ " " ++ y) "" $ map arg args) ++ " = " ++ printExpr expr where
            typeSig = case ty of
                Just t -> var ++ " : " ++ printType t ++ "\n"
                Nothing -> ""
        body = foldr (\x y -> x ++ "\n" ++ y) "" $ map printDef defs
    in headers ++ "\n" ++ body

runIdris :: Module -> IO()
runIdris (Module name d) = do
    writeFile ("out/" ++ name ++ ".idr") $ printIdris (Module name d)