{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Print.Agda
  ( printModule
  , runAgda
  ) where

import Data.List (intercalate)

import Grammar
import Print.Generic

printImport :: Import -> String
printImport (ImportLib NatMod) = "open import Agda.Builtin.Nat"
printImport (ImportLib VecMod) = "open import Data.Vec.Base"
printImport (ImportLib ListMod) = "open import Agda.Builtin.List"
printImport (ImportLib StringMod) = "open import Agda.Builtin.String"

-- Print types
printType :: Type -> String
printType (Con "Type") = "Set"
printType (Con t) = t
printType (Arr t1 t2) = printType t1 ++ " -> " ++ printType t2
printType (TVar t) = t
printType (PCon name types) = name ++ " " ++ unwords (map printType types)
-- Ensure `suc` is printed with parentheses
printType (DCon name [] exprs) = -- For dependent type constructors (like suc)
    name ++ " " ++ unwords (map printExpr exprs)
printType (DCon name types exprs) = -- For dependent type constructors (like suc)
    name ++ " " ++ unwords (map printType types) ++ " " ++ unwords (map printExpr exprs)
printType (Suc t) = "(suc " ++ printType t ++ ")"
printType (Index names ty) = "{" ++ unwords names ++ " : " ++ printType ty ++ "}"

-- Print expressions
printExpr :: Expr -> String
printExpr (Constructor name) = name
printExpr (Var var) = var
printExpr (Nat n) = show n
printExpr (Bool bool) = show bool
printExpr (String str) = "\"" ++ str ++ "\""
printExpr (Paren e) = parens $ printExpr e
printExpr (Mon op e) = parens $ (op ++ printExpr e)
printExpr (Bin op e1 e2) = printExpr e1 ++ " " ++ op ++ " " ++ printExpr e2
printExpr (Let [] expr) = printExpr expr -- this should never happen
printExpr (Let (d:[]) expr) = "let\n    " ++ printDef d ++ " in\n    " ++ printExpr expr
printExpr (Let (d:ds) expr) = "let\n    " ++ printDef d ++ concatMap (\x -> "\n    " ++ printDef x) ds ++ "\n    in " ++ printExpr expr -- Changed foldr to concatMap to preserve order
printExpr (If cond thn els) = "if " ++ printExpr cond ++ " then " ++ printExpr thn ++ " else " ++ printExpr els
printExpr (Where expr ds) = (++) (printExpr expr) $ (++) "\n    where " $ concatMap (\x -> "\n    " ++ printDef x) ds
printExpr (FunCall fun args) = fun ++ " " ++ unwords (map printExpr args) -- Added case for FunCall
printExpr (VecE l) = "([" ++ intercalate " ∷ " (map printExpr l) ++ "])"
printExpr (ListE l) = "([" ++ intercalate " ∷ " (map printExpr l) ++ "])"

-- Function to print variable definitions
printDef :: Definition -> String
printDef (DefVar var ty expr) = typeSig ++ var ++ " = " ++ printExpr expr ++ "\n"
    where
        typeSig = case ty of
            Just t -> var ++ " : " ++ printType t ++ "\n"
            Nothing -> ""

-- Function to print function definitions
printDef (DefFun var ty args expr) = typeSig ++ var ++ " " ++ argsStr ++ " = " ++ printExpr expr
    where
        typeSig = case ty of
            Just t -> var ++ " : " ++ printType t ++ "\n"
            Nothing -> ""
        argsStr = unwords $ map arg args  -- Correctly handle argument names

printDef (DefNesFun var Nothing args expr) = printDef (DefFun var Nothing args expr)
printDef (DefNesFun var (Just t) args expr) = printDef (DefFun var (Just t) args expr)
--Name [(Name,Type)] Type [([Arg], Expr)]
printDef (DefPatt var params ty _ cons) =
    var ++ " : " ++ (printType (foldr (\x y -> Arr x y) ty (map snd params))) ++ unwords (map (\(a,e) -> "\n" ++ var ++ " " ++ (unwords $ map (\(Arg n _) -> n) a) ++ " = " ++ printExpr e) cons) ++ "\n"
-- Function to print datatype definitions
printDef (DefDataType name cons ty) =
  "data " ++ name ++ " : " ++ printType ty ++ " where" ++
  unwords (map (\(n, t) -> "\n " ++ n ++ " : " ++ printType t) cons) ++ "\n"
printDef (DefPDataType name params cons ty) =
  "data " ++ name ++ " " ++
  unwords (map (\(x, y) -> "(" ++ x ++ " : " ++ printType y ++ ") ") params) ++
  " : " ++ printType ty ++
  " where" ++ unwords (map (\(n, t) -> "\n " ++ n ++ " : " ++ printType t) cons) ++ "\n"

-- Function for records
printDef (DefRecType name params consName fields _) =
    "record " ++ name ++ paramsStr ++ " : Set where\n    constructor " ++ consName ++ "\n    field\n" ++
    concatMap (\(fname, ftype) -> "        " ++ fname ++ " : " ++ printType ftype ++ "\n") fields
    where
        paramsStr = case params of
            [] -> ""
            _ -> " " ++ unwords (map (\ (Arg n t) -> parens (n ++ " : " ++ printType t)) params)

printDef (DefRec name recType consName fields) =
    "\n" ++ name ++ " : " ++ printType recType ++ "\n" ++
    name ++ " = " ++ consName ++ concatMap (\(_, value) -> " " ++ printExpr value) fields

printDef (OpenName _) = ""
printDef (DefModule m) = printModule m


-- Print the Agda module
printModule :: Module -> String
printModule (Module name imports defs) =
    let
        headers = "module " ++ name ++ " where \n" ++ unlines (map printImport imports)
        -- Concatenate all definitions
        body = concatMap printDef defs  -- Changed foldr to concatMap to preserve order

    in headers ++ "\n" ++ body

printModule (File name str) = "module " ++ name ++ " where \n" ++ str

runAgda :: Module -> IO()
runAgda m = do
    writeFile ("out/" ++ name ++ ".agda") $ printModule m
    where name = modname m
