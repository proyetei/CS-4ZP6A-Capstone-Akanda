module PrintLean (runLean) where

import Grammar

imports = "import Init.Data.Vector"

-- Print types (unchanged)
printType (Con t) = t
printType (Arr t1 t2) = printType t1 ++ " -> " ++ printType t2
printType (TVar t) = t
printType (PCon "Vec" args) = "Vector " ++ unwords (map printType args)
printType (PCon name types) = name ++ " " ++ unwords (map printType types)
printType (DCon name types exprs) = name ++ " " ++ unwords (map printType types) ++ " " ++ unwords (map printExpr exprs)
printType (Suc t) = "(Nat.succ " ++ printType t ++ ")"  -- Use `Nat.succ` explicitly

printReturnType (Con t) = t
printReturnType (Arr _ t) = printReturnType t

printArg a = "(" ++ (arg a) ++ " : " ++ (printType $ ty a) ++ ")"

-- Print expressions (unchanged)
printExpr (Var var) = var
printExpr (Int int) = show int
printExpr (Bool bool) = show bool
printExpr (String str) = str
printExpr (Paren e) = "(" ++ printExpr e ++ ") "
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

-- Function for records remains unchanged.
printDef _ (DefRecType name params maybeConName fields _) =
    "structure " ++ name ++ paramsStr ++ " where\n    " ++ consName ++ " ::\n" ++
    concatMap (\(fname, ftype) -> "    " ++ fname ++ " : " ++ printType ftype ++ "\n") fields
    where
        consName = case maybeConName of
            Just conName -> conName
            Nothing -> "Const"
        paramsStr = case params of
            Just args -> " " ++ unwords (map (\(Arg n t) -> "(" ++ n ++ " : " ++ printType t ++ ")") args)
            Nothing -> ""

-- modified InitRec printer.
-- It takes a list of record definitions (recs) and uses it to build an open line.
printDef recs (InitRec name recType maybeConsName fields) =
    openLine ++
    name ++ " : " ++ recType ++ " := " ++ consName ++ concatMap (\(_, value) -> " " ++ printExpr value) fields
  where
    recNamesList = [ rName | DefRecType rName _ _ _ _ <- recs ]
    openLine = if null recNamesList then "" else "open " ++ unwords recNamesList ++ "\n"
    consName = case maybeConsName of
                 Just c -> c
                 Nothing -> case lookupConstructor recType recs of
                              Just conName -> conName
                              Nothing -> "Const"
                              
    lookupConstructor :: String -> [Definition] -> Maybe String
    lookupConstructor rt rs =
        case [ c | DefRecType rName _ (Just c) _ _ <- rs, rName == rt ] of
            (c:_) -> Just c
            _     -> Nothing


printLean :: Module -> String
printLean (Module name defs) =
    let
        headers = imports
        recs = [ d | d@(DefRecType _ _ _ _ _) <- defs ]  -- extract record definitions from the module
        body = foldl (\x y -> x ++ "\n" ++ printDef recs y) "" defs
    in headers ++ "\n" ++ body
printLean (File _ str) = str

runLean :: Module -> IO()
runLean m = do
    writeFile ("out/" ++ name ++ ".lean") $ printLean m
  where 
    name = case m of 
           Module n _ -> n
           File n _ -> n
