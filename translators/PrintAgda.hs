module PrintAgda (runAgda) where

import Grammar

imports = "open import Agda.Builtin.IO  \nopen import Agda.Builtin.Nat\n" --open import IO alone not scoping to stlib 

printType (Con t) = t
printType (Arr t1 t2) = printType t1 ++ " -> " ++ printType t2
printExpr (Var var) = var
printExpr (Int int) = show int
printExpr (Bool bool) = show bool
printExpr (String str) = str
printExpr (Mon op e) = "(" ++ op ++ printExpr e ++ ")"
printExpr (Bin op e1 e2) = printExpr e1 ++ " " ++ op ++ " " ++ printExpr e2
printExpr (Let [] expr) = printExpr expr -- this should never happen
printExpr (Let (d:[]) expr) = "let\n    " ++ printDef d ++ "\nin\n    " ++ printExpr expr
printExpr (Let (d:ds) expr) = "let\n    " ++ printDef d ++ concatMap (\x -> "\n    " ++ printDef x) ds ++ "\n    in " ++ printExpr expr -- Changed foldr to concatMap to preserve order
printExpr (If cond thn els) = "if " ++ printExpr cond ++ " then " ++ printExpr thn ++ " else " ++ printExpr els
printExpr (Where expr ds) = (++) (printExpr expr) $ (++) "\n    where " $ concatMap (\x -> "\n    " ++ printDef x) ds
printExpr (FunCall fun args) = fun ++ " " ++ unwords (map printExpr args) -- Added case for FunCall

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
