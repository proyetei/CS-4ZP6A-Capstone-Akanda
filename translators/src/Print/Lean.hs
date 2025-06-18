module Print.Lean
  ( printModule
  , runLean
  ) where

import Data.List (intercalate)

import Grammar
import Print.Generic

import_, univ, arr, typedel, assign :: String
import_ = "import "
univ = "Type"
arr = " -> "
typedel = " : "
assign = " := "

printImport :: Import -> String
printImport (ImportLib VecMod) = import_ ++ "Init.Data.Vector"
-- There rest are builtin
printImport (ImportLib NatMod) = ""
printImport (ImportLib StringMod) = ""
printImport (ImportLib ListMod) = ""

-- Print types (unchanged)
printType :: Type -> String
printType (Univ) = univ
printType (Con t) = t
printType (Arr t1 t2) = printType t1 ++ arr ++ printType t2
printType (TVar t) = t
printType (PCon "Vec" args) = "Vector " ++ unwords (map printType args)
printType (PCon name types) = name ++ " " ++ unwords (map printType types)
printType (DCon name [] exprs) = name ++ " " ++ unwords (map printExpr exprs)
printType (DCon name types exprs) = name ++ " " ++ unwords (map printType types) ++ " " ++ unwords (map printExpr exprs)
printType (Index names ty) = brackets $ unwords names ++ typedel ++ printType ty
printType (Embed e) = printExpr e

printReturnType :: Type -> String
printReturnType (Con t) = t
printReturnType (Arr _ t) = printReturnType t
printReturnType _ = error "show not occur as a return type"

printArg :: Arg -> String
printArg a = parens $ (arg a) ++ typedel ++ (printType $ argty a)

-- Print expressions (unchanged)
printExpr :: Expr -> String
printExpr (Constructor name) = name
printExpr (Var var) = var
printExpr (Nat n) = show n
printExpr (String str) = quote str
printExpr (Paren e) = parens $ printExpr e
printExpr (Bin op e1 e2) = printExpr e1 ++ printOp op ++ printExpr e2
printExpr (Let [] expr) = printExpr expr
printExpr (Let (d:[]) expr) = line ("let " ++ (printDef [] d)) ++ printExpr expr
printExpr (Let (d:ds) expr) = "let " ++ intercalate "\nlet " (map (printDef []) (d:ds)) ++ "\n" ++ printExpr expr
printExpr (If cond thn els) = "if " ++ printExpr cond ++ " then \n\t" ++ printExpr thn ++ "\nelse " ++ printExpr els
printExpr (Where expr ds) = printExpr expr ++ "\n\twhere " ++ intercalate "\n\t" (map (printDef []) ds)
printExpr (FunCall fun args) = fun ++ " " ++ unwords (map printExpr args)
printExpr (VecE l) = '#' : sqbrackets (intercalate ", " (map printExpr l))
printExpr (ListE l) = sqbrackets $ intercalate ", " (map printExpr l)
printExpr (Suc t) = parens $ "Nat.succ " ++ printExpr t  -- Use `Nat.succ` explicitly

printOp :: Op -> String
printOp Plus = " + "

printDef :: [Definition] -> Definition -> String
printDef _ (DefUVar var expr) = var ++ assign ++ printExpr expr
printDef _ (DefTVar var t expr) = "def " ++ var ++ typedel ++ printType t ++ assign ++ printExpr expr
printDef _ (DefFun var Nothing args expr) = var ++ " " ++ (intercalate " " $ map arg args) ++ assign ++ printExpr expr
printDef _ (DefFun var (Just t) args expr) = "def " ++ var ++ " " ++ (intercalate " " $ map printArg args) ++ typedel ++
  printType t ++ assign ++ printExpr expr
printDef _ (DefNesFun var Nothing args expr) = var ++ " " ++ unwords (map arg args) ++ assign ++ printExpr expr
printDef _ (DefNesFun var (Just t) args expr) = var ++ " " ++ unwords (map printArg args) ++ typedel ++ printReturnType t ++ assign ++ printExpr expr
printDef _ (DefPatt var params ty _ cons) =
    "def " ++ var ++ typedel ++ printType (foldr Arr ty (map snd params)) ++
    unwords (map (\(a,e) -> "\n| " ++ (unwords $ map arg a) ++ " => " ++ printExpr e ) cons)
printDef _ (DefDataType str args t) = "inductive " ++ str ++ typedel ++ printType t ++ " where " ++ unwords (map (\(x, y) -> "\n| " ++ x ++ typedel ++ printType y) args)
printDef _ (DefPDataType str params args t) =
   "inductive " ++ str ++ " " ++ unwords (map (\(x, y) -> parens (x ++ typedel ++ printType y)) params) ++ typedel ++
   printType t ++ " where " ++ unwords (map (\(x, y) -> "\n| " ++ x ++ typedel ++ (printType y)) args)

-- records Def
printDef _ (DefRecType name params consName fields _) =
    "structure " ++ name ++ paramsStr ++ " where\n    " ++ consName ++ " ::\n" ++
    concatMap (\(fname, ftype) -> "    " ++ fname ++ " : " ++ printType ftype ++ "\n") fields
    where
        paramsStr = case params of
            [] -> ""
            _ -> " " ++ unwords (map (\(Arg n t) -> parens $ n ++ typedel ++ printType t) params)


-- OpenLine: It takes a list of record definitions (recs) and uses it to build an open line.
-- Exclusive lean syntax needed for simplicity
printDef recs (DefRec name recType consName fields) =
    openLine ++
    name ++ typedel ++ printType recType ++ assign ++ consName ++ " " ++ intercalate " " (map (printExpr . snd) fields)
  where
    recNamesList = [ rName | DefRecType rName _ _ _ _ <- recs ]
    openLine = if null recNamesList then "" else "open " ++ unwords recNamesList ++ "\n"
printDef _ (OpenName n) = "open " ++ n
printDef _ (DefModule m) = printModule m


printModule :: Module -> String
printModule (Module _ imports defs) =
    let
        headers = unlines (map printImport imports)
        recs = [ d | d@(DefRecType _ _ _ _ _) <- defs ]  -- extract record definitions from the module
        body = intercalate "\n" (map (printDef recs) defs)
    in headers ++ "\n" ++ body

printModule (File _ str) = str

runLean :: Module -> IO()
runLean m = writeFile ("out/" ++ modname m ++ ".lean") $ printModule m
