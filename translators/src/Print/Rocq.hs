module Print.Rocq
  ( printModule
  , runRocq
  ) where

import Data.Char
import Data.List (isPrefixOf, intercalate)

import Grammar
import Print.Generic

import_, univ, arr, typedel, assign :: String
import_ = "Require Import " -- separate?
univ = "Type"
arr = " -> "
typedel = " : "
assign = " := "


printImport :: Import -> String
printImport (ImportLib VecMod) = line (import_ ++ "Coq.Vectors.Vector. ") ++ "Import VectorNotations."
printImport (ImportLib StringMod) = import_ ++ "Coq.Strings.String."
-- the rest are builtin
printImport (ImportLib NatMod) = ""
printImport (ImportLib ListMod) = ""

printType :: Type -> String
printType (Univ) = univ
printType (Con t) = if  "Cap_" `isPrefixOf` t ||
                        "Record" `isPrefixOf` t
                        then t else (map toLower t) -- if starts with keyword Cap_ maintain, else lower case
printType (Arr t1 t2) = printType t1 ++ arr ++ printType t2
printType (TVar t) = t
printType (PCon "Vec" args) = "t " ++ unwords (map printType args)
printType (PCon name types) = (map toLower name) ++ " " ++ unwords (map printType types)
printType (DCon name [] exprs) = name ++ " " ++ unwords (map printExpr exprs)
printType (DCon name types exprs) = name ++ " " ++ unwords (map printType types) ++ " " ++ unwords (map printExpr exprs)
printType (Index names ty) = "forall " ++ brackets (map toLower (unwords names) ++ typedel ++ printType ty)
printType (Embed e) = printExpr e

printReturnType :: Type -> String
printReturnType (Con t) = map toLower t --required for nested functions
printReturnType (Arr _ t) = printReturnType t
printReturnType _ = error "should not occur as a return type"

printArg :: Arg -> String
printArg a = parens $ (arg a) ++ typedel ++ (printType $ argty a)

printExpr :: Expr -> String
printExpr (Constructor name) = map toLower name
printExpr (Var var) = var
printExpr (Nat n) = show n
printExpr (String str) = quote str
printExpr (Paren e) = parens $ printExpr e
printExpr (Bin op e1 e2) = printExpr e1 ++ printOp op ++ printExpr e2
printExpr (Let ds expr) =
    foldl (\acc def -> acc ++ "let " ++ printDef def ++ " in\n    ") "" ds ++ printExpr expr --modified for proper let-in nesting
printExpr (If cond thn els) = "if " ++ printExpr cond ++ " then " ++ printExpr thn ++ " else " ++ printExpr els
printExpr (Where expr ds) = line (printExpr expr) ++ "\twhere " ++ foldl (\x y -> x ++ "\n\t" ++ y) "" (map printDef ds)
printExpr (FunCall fun args) = fun ++ " " ++ (unwords $ map printExpr args) -- Added case for FunCall
printExpr (VecE l) = sqbrackets $ intercalate "; " (map printExpr l)
printExpr (ListE l) = sqbrackets $ intercalate ", " (map printExpr l)
printExpr (Suc e) = parens $ "S " ++ printExpr e

printOp :: Op -> String
printOp Plus = " + "

printDef :: Definition -> String
printDef (DefUVar var expr) = var ++ assign ++ printExpr expr
printDef (DefTVar var t expr) = 
  line $ "Definition " ++ var ++ typedel ++ printType t ++ assign ++ printExpr expr ++ ". "
printDef (DefFun var Nothing args expr) = var ++ (foldl (\x y -> x ++ " " ++ y) "" $ map arg args) ++ assign ++ printExpr expr
printDef (DefFun var (Just t) args expr) = "Definition " ++ var ++ 
  (foldl (\x y -> x ++ " " ++ y) "" $ map printArg args) ++ typedel ++ printType t ++ assign ++ printExpr expr ++ "."
printDef (DefNesFun var Nothing args expr) = var ++ " " ++ (unwords $ map arg args) ++ assign ++ printExpr expr
printDef (DefNesFun var (Just t) args expr) = var ++ " " ++ (unwords $ map printArg args) ++ typedel ++
  printReturnType t ++ assign ++ printExpr expr

printDef (DefPatt var params ty m cons) = "Fixpoint " ++ var ++ " " ++ 
  (unwords $ map (\(x, y) -> " " ++ parens (x ++ typedel ++ printType y)) params) ++ 
  typedel ++ printType ty ++ assign ++ 
  "\nmatch " ++ m ++ " with" ++
   unwords (map (\(a, e) -> "\n| " ++ (unwords $ map (map toLower . arg) a) ++ " => " ++ printExpr e) cons) ++ " end."
printDef (DefDataType name args ty) = let
    printIndices :: Type -> String
    printIndices (Arr (Index n t) ctype) = printType(Index n t)  ++ ", " ++ printType ctype
    printIndices t = printType t
    in
        "Inductive " ++ map toLower name ++ typedel ++ printType ty ++ assign ++ unwords (map (\(x, y) -> "\n| " ++ map toLower x ++ " : " ++ printIndices y) args) ++ "."
printDef (DefPDataType name params args ty) = let
    printIndices :: Type -> String
    printIndices (Arr (Index n t) ctype) = printType(Index n t)  ++ ", " ++ printType ctype
    printIndices t = printType t
    in
        "Inductive " ++ map toLower name ++
         unwords (map (\(x, y) -> " " ++ parens (map toLower x ++ typedel ++ printType y)) params) ++
         typedel ++ printType ty ++ assign ++
         unwords (map (\(x, y) -> "\n| " ++ map toLower x ++ typedel ++ printIndices y) args) ++ "."

--Function for Records
printDef (DefRecType name params consName fields _) =
    "Record " ++ name ++ paramsStr ++ typedel ++ printType Univ ++ assign ++ consName ++ " " ++
    line (brackets ("\n" ++ concatMap (\(fname, ftype) -> line $ "  " ++ fname ++ typedel ++ printType ftype ++ ";") fields) ++ ".")
    where
        paramsStr = case params of
            [] -> ""
            _ -> " " ++ unwords (map (\(Arg n t) -> parens $ n ++ " : " ++ printType t) params)

printDef (DefRec name recType consName fields) =
  line ("Definition " ++ name ++ typedel ++ printType recType ++ assign) ++ "  " ++ line (constructorCall ++ ".")
  where
    -- Split the record type into words.
    -- The first word is the record name; any remaining words are the concrete parameter values.
    recWords = words $ printType recType
    paramsStr = case recWords of
                  (_:ps) -> unwords ps
                  _      -> ""

    -- Use the provided constructor name if available; otherwise, look up the default.

    -- Build a string for the field values; each field value is preceded by a space.
    fieldsStr = concatMap (\(_, value) -> " " ++ printExpr value) fields

    -- If there are parameters, insert them between the constructor name and the field values.
    constructorCall = if null paramsStr
                      then consName ++ fieldsStr
                      else consName ++ " " ++ paramsStr ++ fieldsStr
printDef (OpenName _) = ""
printDef (DefModule m) = printModule m
printDef (Separator c n b) =
  let s = replicate (fromIntegral n) c in
  if b then '\n' : line s else s


printModule :: Module -> String
printModule (Module name imports defs) =
    let
        headers = unlines (map printImport imports) ++ "\n\nModule " ++ name ++ ".\n"
        body = foldl (\x y -> x ++ "\n" ++ y) "" $ map printDef defs
    in headers ++ "\n" ++ body ++ "\nEnd " ++ name ++ "."

runRocq :: Module -> IO()
runRocq m = writeFile ("out/" ++ modname m ++ ".v") $ printModule m
