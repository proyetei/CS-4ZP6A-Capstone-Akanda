module PrintLean (runLean) where

import Grammar

printImport (ImportLib "Vec") = "import Init.Data.Vector"
printImport (ImportLib _) = ""
printImport (ImportFun name lib) = "open import " ++ lib ++ " using (" ++ name ++ ")"

printImport (ImportLib "Vec") = "import Init.Data.Vector"
printImport (ImportLib _) = ""
printImport (ImportFun name lib) = "open import " ++ lib ++ " using (" ++ name ++ ")"
-- Print types (unchanged)
printType (Con t) = t
printType (Arr t1 t2) = printType t1 ++ " -> " ++ printType t2
printType (TVar t) = t
printType (PCon "Vec" args) = "Vector " ++ unwords (map printType args)
printType (PCon name types) = name ++ " " ++ unwords (map printType types)
printType (DCon name [] exprs) = -- For dependent type constructors (like suc)
    name ++ " " ++ unwords (map printExpr exprs)
printType (DCon name types exprs) = name ++ " " ++ unwords (map printType types) ++ " " ++ unwords (map printExpr exprs)
printType (Suc t) = "(Nat.succ " ++ printType t ++ ")"  -- Use `Nat.succ` explicitly
printType (Index names ty) = "{" ++ unwords names ++ " : " ++ printType ty ++ "}"

printReturnType (Con t) = t
printReturnType (Arr _ t) = printReturnType t

printArg a = "(" ++ (arg a) ++ " : " ++ (printType $ ty a) ++ ")"

-- Print expressions (unchanged)
printExpr (Constructor name) = name
printExpr (Var var) = var
printExpr (Int int) = show int
printExpr (Bool bool) = show bool
printExpr (String str) = str
printExpr (Paren e) = "(" ++ printExpr e ++ ")"
printExpr (Mon op e) = "(" ++ op ++ printExpr e ++ ")"
printExpr (Bin op e1 e2) = printExpr e1 ++ " " ++ op ++ " " ++ printExpr e2
printExpr (Let [] expr) = printExpr expr
printExpr (Let (d:[]) expr) = "let " ++ (printDef [] d) ++ "\n" ++ printExpr expr
printExpr (Let (d:ds) expr) = "let " ++ (foldl (\x y -> x ++ "\nlet " ++ y) (printDef [] d) $ map (printDef []) ds) ++ "\n" ++ printExpr expr
printExpr (If cond thn els) = "if " ++ printExpr cond ++ " then \n\t" ++ printExpr thn ++ "\nelse " ++ printExpr els
printExpr (Where expr ds) = printExpr expr ++ "\n\twhere " ++ foldl (\x y -> x ++ "\n\t" ++ y) "" (map (printDef []) ds)
printExpr (FunCall fun args) = fun ++ " " ++ unwords (map printExpr args)
printExpr (VecEmpty) = "#[]"  -- Lean uses `#[]` for empty vectors
printExpr (VecCons expr rest) = "⟨#[" ++ printVecElements (VecCons expr rest) ++ "], rfl⟩"
printExpr (ListEmpty) = "[]"
printExpr (ListCons expr rest) = "[" ++ printListElements (ListCons expr rest) ++ "]"
-- Helper for vectors
printVecElements :: Expr -> String
printVecElements VecEmpty = ""
printVecElements (VecCons x VecEmpty) = printExpr x
printVecElements (VecCons x xs) = printExpr x ++ ", " ++ printVecElements xs
printListElements ListEmpty = ""
printListElements (ListCons expr ListEmpty) = printExpr expr
printListElements (ListCons expr rest) = printExpr expr ++ ", " ++ printListElements rest
printDef _ (DefVar var Nothing expr) = var ++ " := " ++ printExpr expr
printDef _ (DefVar var (Just t) expr) = "def " ++ var ++ " : " ++ printType t ++ " := " ++ printExpr expr
printDef _ (DefFun var Nothing args expr) = var ++ (foldl (\x y -> x ++ " " ++ y) "" $ map arg args) ++ " := " ++ printExpr expr
printDef _ (DefFun var (Just t) args expr) = "def " ++ var ++ (foldl (\x y -> x ++ " " ++ y) "" $ map printArg args) ++ " : " ++ printType t ++ " := " ++ printExpr expr
printDef _ (DefNesFun var Nothing args expr) = var ++ " " ++ unwords (map arg args) ++ " := " ++ printExpr expr
printDef _ (DefNesFun var (Just t) args expr) = var ++ " " ++ unwords (map printArg args) ++ " : " ++ printReturnType t ++ " := " ++ printExpr expr
printDef _ (DefPatt var params ty _ cons) = 
    "def " ++ var ++ " : " ++ printType (foldr (\x y -> Arr x y) ty (map snd params)) ++ unwords (map (\(a,e) -> "\n| " ++ (unwords $ map (\(Arg name _) -> name) a) ++ " => " ++ printExpr e ) cons)
printDef _ (DefDataType str args t) = "inductive " ++ str ++ " : " ++ printType t ++ " where " ++ unwords (map (\(x, y) -> "\n| " ++ x ++ " : " ++ printType y) args)
printDef _ (DefPDataType str params args t) =
   "inductive " ++ str ++ unwords (map (\(x, y) -> " (" ++ x ++ ": " ++ printType y ++ ")") params) ++ " where " ++ unwords (map (\(x, y) -> "\n| " ++ x ++ " : " ++ (printType y)) args)

-- records Def
printDef _ (DefRecType name params consName fields _) =
    "structure " ++ name ++ paramsStr ++ " where\n    " ++ consName ++ " ::\n" ++
    concatMap (\(fname, ftype) -> "    " ++ fname ++ " : " ++ printType ftype ++ "\n") fields
    where
        paramsStr = case params of
            [] -> ""
            _ -> " " ++ unwords (map (\(Arg n t) -> "(" ++ n ++ " : " ++ printType t ++ ")") params)


-- OpenLine: It takes a list of record definitions (recs) and uses it to build an open line.
-- Exclusive lean syntax needed for simplicity
printDef recs (DefRec name recType consName fields) =
    openLine ++
    name ++ " : " ++ printType recType ++ " := " ++ consName ++ concatMap (\(_, value) -> " " ++ printExpr value) fields
  where
    recNamesList = [ rName | DefRecType rName _ _ _ _ <- recs ]
    openLine = if null recNamesList then "" else "open " ++ unwords recNamesList ++ "\n"
printDef _ (OpenName n) = "open " ++ n


printLean :: Module -> String
printLean (Module name imports defs) =
    let
        headers = unlines (map printImport imports)
        recs = [ d | d@(DefRecType _ _ _ _ _) <- defs ]  -- extract record definitions from the module
        body = foldl (\x y -> x ++ "\n" ++ printDef recs y) "" defs
    in headers ++ "\n" ++ body
printLean (File _ str) = str

runLean :: Module -> IO()
runLean m = do
    writeFile ("out/" ++ name ++ ".lean") $ printLean m
  where 
    name = case m of 
           Module n _ _ -> n
           File n _ -> n
