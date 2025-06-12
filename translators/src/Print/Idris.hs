{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Print.Idris (runIdris) where

import Data.List (intercalate)

import Grammar

printImport :: Import -> String
printImport (ImportLib "Vec") = "import Data.Vect"
printImport (ImportLib _) = ""
printImport (ImportFun name lib) = "open import " ++ lib ++ " using (" ++ name ++ ")"

printType :: Type -> String
printType (Con t) = t
printType (Arr t1 t2) = printType t1 ++ " -> " ++ printType t2
printType (TVar t) = t
printType (PCon "Vec" [Con baseType, size]) = "Vect " ++ printType size ++ " " ++ baseType
printType (PCon name types) = name ++ " " ++ unwords (map printType types)
printType (DCon name [] exprs) = -- For dependent type constructors (like suc)
    name ++ " " ++ unwords (map printExpr exprs)
printType (DCon name types exprs) = -- For dependent type constructors (like suc)
    name ++ " " ++ unwords (map printType types) ++ " " ++ unwords (map printExpr exprs)
printType (Suc t) = "(S " ++ printType t ++ ")"
printType (Index names ty) = "{" ++ unwords' names ++ " : " ++ printType ty ++ "}" where
    unwords' [] = ""
    unwords' [n] = n
    unwords' more = foldl1 (\ x n -> x ++ ", " ++ n) more

printExpr :: Expr -> String
printExpr (Constructor name) = name
printExpr (Var var) = var
printExpr (Int int) = show int
printExpr (Bool bool) = show bool
printExpr (String str) = "\"" ++ str ++ "\""
printExpr (Paren e) = "(" ++ printExpr e ++ ")"
printExpr (Mon op e) = "(" ++ op ++ printExpr e ++ ")"
printExpr (Bin op e1 e2) = printExpr e1 ++ " "  ++ op ++ " " ++ printExpr e2
printExpr (Let [] expr) = printExpr expr -- this should never happen
printExpr (Let (d:[]) expr) = "let \n    " ++ (printDef d) ++ " in \n    " ++ printExpr expr
printExpr (Let (d:ds) expr) = "let \n    " ++ (foldl (\x y -> x ++ "\n    " ++ y) (printDef d) $ map printDef ds) ++ "\n    in \n    " ++ printExpr expr -- probably need to recursively indent blocks to make sure everything stays aligned
printExpr (If cond thn els) = "if " ++ printExpr cond ++ " then " ++ printExpr thn ++ " else " ++ printExpr els
printExpr (Where expr ds) = (++) (printExpr expr) $ (++) "\n    where " $ foldl (\x y -> x ++ "\n    " ++ y) "" $ map printDef ds
printExpr (FunCall fun args) = fun ++ " " ++ (unwords $ map printExpr args) -- Added case for FunCall
printExpr (VecE l) = "[" ++ intercalate ", " (map printExpr l) ++ "]"
printExpr (ListE l) = "[" ++ intercalate ", " (map printExpr l) ++ "]"


printDef :: Definition -> String
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
printDef (DefPatt var params ty _ cons) =
    var ++ " : " ++ printType (foldr (\x y -> Arr x y) ty (map snd params)) ++ unwords (map (\(a,e) -> "\n" ++ var ++ " " ++ (unwords $ map (\(Arg name _) -> name) a) ++ " = " ++ printExpr e ) cons)
printDef (DefDataType name cons ty) = "data " ++ name ++ " : " ++ printType ty ++ " where" ++ unwords (map (\(n, t) -> "\n " ++ n ++ " : " ++ printType t) cons) ++ "\n"
printDef (DefPDataType name params cons ty) = "data " ++ name ++ " : " ++ foldr (\(x, y) z -> "(" ++ x ++ " : " ++ printType y ++ ") -> " ++ z) (printType ty) params ++ " where" ++ unwords (map (\(n, t) -> "\n " ++ n ++ " : " ++ unwords (map (\(p, _) -> p ++ " ->") params) ++ printType t) cons) ++ "\n"

-- Record Defn
printDef (DefRecType name params consName fields _) =
    "record " ++ name ++ paramsStr ++ " where\n    constructor " ++ consName ++ "\n" ++
    unlines (map (\(fname, ftype) -> "    " ++ fname ++ " : " ++ printType ftype) fields)
  where
    paramsStr = case params of
        [] -> ""
        _ -> " " ++ unwords (map (\(Arg n t) -> "(" ++ n ++ " : " ++ printType t ++ ")") params)

printDef (DefRec name recType consName fields) =
    name ++ " : " ++ printType recType ++
    "\n" ++ name ++ " = " ++ consName ++ concatMap (\(_, value) -> " " ++ printExpr value) fields ++ "\n"

printDef (OpenName _) = ""
printDef (DefModule m) = printModule m
-- Catch-all to prevent non-exhaustive errors
printDef _ = error "Unhandled case in printDef"

printModule :: Module -> String
printModule (Module _ imports defs) =
    let
        headers = "module Main\n" ++ unlines (map printImport imports)
        body = foldl (\x y -> x ++ "\n" ++ y) "" $ map printDef defs
    in headers ++ "\n" ++ body ++ "\nmain : IO()\nmain = putStrLn \"\""

printModule (File _ str) = "module Main\n" ++ str ++ "\nmain : IO()\nmain = putStrLn \"\""

runIdris :: Module -> IO()
runIdris m = do
    writeFile ("out/" ++ name ++ ".idr") $ printModule m
        where
            name = case m of
                Module n _ _ -> n
                File n _ -> n
