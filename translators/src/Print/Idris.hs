module Print.Idris
  ( printModule
  , runIdris
  ) where

import Data.List (intercalate)

import Grammar
import Print.Generic

import_, univ, arr, typedel, assign :: String
import_ = "import "
univ = "Type"
arr = " -> "
typedel = " : "
assign = " = "

printImport :: Import -> String
printImport (ImportLib VecMod) = import_ ++ "Data.Vect"
-- There rest are builtin
printImport (ImportLib NatMod) = ""
printImport (ImportLib StringMod) = ""
printImport (ImportLib ListMod) = ""

printType :: Type -> String
printType (Univ) = univ
printType (Con t) = t
printType (Arr t1 t2) = printType t1 ++ arr ++ printType t2
printType (TVar t) = t
printType (PCon "Vec" [Con baseType, size]) = "Vect " ++ printType size ++ " " ++ baseType
printType (PCon name types) = name ++ " " ++ unwords (map printType types)
printType (DCon name [] exprs) = -- For dependent type constructors
    name ++ " " ++ unwords (map printExpr exprs)
printType (DCon name types exprs) = -- For dependent type constructors
    name ++ " " ++ unwords (map printType types) ++ " " ++ unwords (map printExpr exprs)
printType (Index names ty) = brackets $ intercalate ", " names ++ " : " ++ printType ty
printType (Embed e) = printExpr e

printExpr :: Expr -> String
printExpr (Constructor name) = name
printExpr (Var var) = var
printExpr (Nat n) = show n
printExpr (String str) = quote str
printExpr (Paren e) = parens $ printExpr e
printExpr (Bin op e1 e2) = printExpr e1 ++ printOp op ++ printExpr e2
printExpr (Let [] expr) = printExpr expr -- this should never happen
printExpr (Let (d:[]) expr) = "let \n    " ++ (printDef d) ++ " in \n    " ++ printExpr expr
printExpr (Let (d:ds) expr) = 
  "let \n    " ++ intercalate "\n    " (map printDef (d:ds)) ++
  "\n    in \n    " ++ printExpr expr -- probably need to recursively indent blocks to make sure everything stays aligned
printExpr (If cond thn els) = "if " ++ printExpr cond ++ " then " ++ printExpr thn ++ " else " ++ printExpr els
printExpr (Where expr ds) = printExpr expr ++ "\n    where " ++ intercalate "\n    " (map printDef ds)
printExpr (FunCall fun args) = fun ++ " " ++ (unwords $ map printExpr args) -- Added case for FunCall
printExpr (VecE l) = sqbrackets $ intercalate ", " (map printExpr l)
printExpr (ListE l) = sqbrackets $ intercalate ", " (map printExpr l)
printExpr (Suc t) = parens $ "S " ++ printExpr t

printOp :: Op -> String
printOp Plus = " + "


printDef :: Definition -> String
printDef (DefUVar var expr) = var ++ assign ++ printExpr expr
printDef (DefTVar var t expr) = line (var ++ typedel ++ printType t) ++ var ++ assign ++ printExpr expr
printDef (DefFun var ty args expr) = typeSig ++ var ++ " " ++ (intercalate " " $ map arg args) ++ assign ++ printExpr expr
    where typeSig = maybe "" (\t -> var ++ typedel ++ printType t ++ "\n    ") ty
printDef (DefNesFun var Nothing args expr) = printDef (DefFun var Nothing args expr)
printDef (DefNesFun var (Just t) args expr) = printDef (DefFun var (Just t) args expr)
printDef (DefPatt var params ty _ cons) =
    var ++ typedel ++ printType (foldr Arr ty (map snd params)) ++
    unwords (map (\(a,e) -> "\n" ++ var ++ " " ++ (unwords $ map arg a) ++ assign ++ printExpr e ) cons)
printDef (DefDataType name cons ty) = "data " ++ name ++ typedel ++ printType ty ++ " where" ++
    (line $ unwords (map (\(n, t) -> "\n " ++ n ++ typedel ++ printType t) cons))
printDef (DefPDataType name params cons ty) = "data " ++ name ++ typedel ++
    foldr (\(x, y) z -> parens (x ++ typedel ++ printType y) ++ arr ++ z) (printType ty) params ++ " where" ++ 
    unwords (map (\(n, t) -> "\n " ++ n ++ typedel ++ line (intercalate arr (map fst params) ++ arr ++ printType t)) cons)

-- Record Defn
printDef (DefRecType name params consName fields _) =
    "record " ++ name ++ paramsStr ++ " where\n    constructor " ++ consName ++ "\n" ++
    unlines (map (\(fname, ftype) -> "    " ++ fname ++ typedel ++ printType ftype) fields)
  where
    paramsStr = case params of
        [] -> ""
        _ -> " " ++ unwords (map (\(Arg n t) -> parens $ n ++ typedel ++ printType t) params)

printDef (DefRec name recType consName fields) =
    (line $ name ++ typedel ++ printType recType) ++
    name ++ assign ++ consName ++ " " ++ (line $ intercalate " " (map (printExpr . snd) fields))

printDef (OpenName _) = ""
printDef (DefModule m) = printModule m
printDef (Separator c n b) =
  let s = replicate (fromIntegral n) c in
  if b then '\n' : line s else s


printModule :: Module -> String
printModule (Module _ imports defs) =
    let
        headers = line ("module Main") ++ unlines (map printImport imports)
        body = intercalate "\n" $ map printDef defs
            -- foldl (\x y -> x ++ "\n" ++ y) "" $ map printDef defs
    in headers ++ "\n" ++ body ++ "\nmain : IO()\nmain = putStrLn \"\""

runIdris :: Module -> IO()
runIdris m = writeFile ("out/" ++ modname m ++ ".idr") $ printModule m
