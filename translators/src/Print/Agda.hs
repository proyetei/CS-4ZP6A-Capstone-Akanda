module Print.Agda
  ( printModule
  , runAgda
  ) where

import Data.List (intercalate)

import Grammar
import Print.Generic

-- keywords
import_, univ, arr, typedel, assign, data_, rec :: String
import_ = "open import " -- separate?
univ = "Set"
arr = " -> "
typedel = " : "
assign = " = "
data_ = "data "
rec = "record "

printImport :: Import -> String
printImport (ImportLib NatMod) = import_ ++ "Agda.Builtin.Nat"
printImport (ImportLib VecMod) = import_ ++ "Data.Vec.Base"
printImport (ImportLib ListMod) = import_ ++ "Agda.Builtin.List"
printImport (ImportLib StringMod) = import_ ++ "Agda.Builtin.String"

-- Print types
printType :: Type -> String
printType (Univ) = univ
printType (Con t) = t
printType (Arr t1 t2) = printType t1 ++ arr ++ printType t2
printType (TVar t) = t
printType (PCon name types) = name ++ " " ++ unwords (map printType types)
-- Ensure `suc` is printed with parentheses
printType (DCon name [] exprs) = -- For dependent type constructors (like suc)
    name ++ " " ++ unwords (map printExpr exprs)
printType (DCon name types exprs) = -- For dependent type constructors (like suc)
    name ++ " " ++ unwords (map printType types) ++ " " ++ unwords (map printExpr exprs)
printType (Index names ty) = brackets $ unwords names ++ typedel ++ printType ty
printType (Embed e) = printExpr e

-- Print expressions
printExpr :: Expr -> String
printExpr (Constructor name) = name
printExpr (Var var) = var
printExpr (Nat n) = show n
printExpr (String str) = quote str
printExpr (Paren e) = parens $ printExpr e
printExpr (Bin op e1 e2) = printExpr e1 ++ printOp op ++ printExpr e2
printExpr (Let ds expr) = 
  "let\n    " ++ (intercalate "\n    " (map printDef ds)) ++ " in\n    " ++ printExpr expr
printExpr (If cond thn els) = "if " ++ printExpr cond ++ " then " ++ printExpr thn ++ " else " ++ printExpr els
printExpr (Where expr ds) = printExpr expr ++ "\n    where " ++ intercalate "\n    "  (map printDef ds)
printExpr (FunCall fun args) = fun ++ " " ++ unwords (map printExpr args) -- Added case for FunCall
printExpr (VecE l) = parens $ sqbrackets $ intercalate " ∷ " (map printExpr l)
printExpr (ListE l) = parens $ sqbrackets $ intercalate " ∷ " (map printExpr l)
printExpr (Suc t) = parens $ "suc " ++ printExpr t

printOp :: Op -> String
printOp Plus = " + "

-- Function to print variable definitions
printDef :: Definition -> String
printDef (DefTVar var t expr) = 
  line (var ++ typedel ++ printType t) ++ line ( var ++ assign ++ printExpr expr)
printDef (DefUVar var expr) = line $ var ++ assign ++ printExpr expr

-- Function to print function definitions
printDef (DefFun var ty args expr) = typeSig ++ var ++ " " ++ argsStr ++ assign ++ printExpr expr
    where
        typeSig = case ty of
            Just t -> line $ var ++ typedel ++ printType t
            Nothing -> ""
        argsStr = unwords $ map arg args  -- Correctly handle argument names

printDef (DefNesFun var Nothing args expr) = printDef (DefFun var Nothing args expr)
printDef (DefNesFun var (Just t) args expr) = printDef (DefFun var (Just t) args expr)
--Name [(Name,Type)] Type [([Arg], Expr)]
printDef (DefPatt var params ty _ cons) =
    var ++ typedel ++ (printType (foldr Arr ty (map snd params))) ++ 
    (line $ unwords (map (\(a,e) -> "\n" ++ var ++ " " ++ (unwords $ map arg a) ++ assign ++ printExpr e) cons))
-- Function to print datatype definitions
printDef (DefDataType name cons ty) =
  data_ ++ name ++ typedel ++ printType ty ++ " where" ++
  (line $ unwords (map (\(n, t) -> "\n " ++ n ++ typedel ++ printType t) cons))
printDef (DefPDataType name params cons ty) =
  data_ ++ name ++ " " ++
  unwords (map (\(x, y) -> parens $ x ++ typedel ++ printType y) params) ++ typedel ++
  printType ty ++
  " where" ++ (line $ unwords (map (\(n, t) -> "\n " ++ n ++ typedel ++ printType t) cons))

-- Function for records
printDef (DefRecType name params consName fields _) =
    rec ++ name ++ paramsStr ++ typedel ++ 
    printType Univ ++ (line " where") ++ 
    "    constructor " ++ line (consName ++ "\n    field") ++
    concatMap (\(fname, ftype) -> line $ "        " ++ fname ++ typedel ++ printType ftype) fields
    where
        paramsStr = case params of
            [] -> ""
            _ -> " " ++ unwords (map (\ (Arg n t) -> parens (n ++ typedel ++ printType t)) params)

printDef (DefRec name recType consName fields) =
    "\n" ++ name ++ typedel ++ line (printType recType) ++
    name ++ assign ++ consName ++ " " ++ intercalate " " (map (printExpr . snd) fields)

printDef (OpenName _) = ""
printDef (DefModule m) = printModule m


-- Print the Agda module
printModule :: Module -> String
printModule (Module name imports defs) =
    let
        headers = "module " ++ name ++ " where\n" ++ unlines (map printImport imports)
        -- Concatenate all definitions
        body = concatMap printDef defs  -- Changed foldr to concatMap to preserve order

    in line headers ++ body

printModule (File name str) = line ("module " ++ name ++ " where") ++ str

runAgda :: Module -> IO()
runAgda m = do
    writeFile ("out/" ++ name ++ ".agda") $ printModule m
    where name = modname m
