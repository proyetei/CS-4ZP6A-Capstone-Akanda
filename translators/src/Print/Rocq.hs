{-# Language OverloadedStrings #-}
module Print.Rocq
  ( printModule
  , render
  , runRocq
  ) where

import Data.Char (toLower)
import Data.List (isPrefixOf, intercalate)

import Prettyprinter
import Prettyprinter.Render.String (renderString)

import Grammar
import qualified Print.Generic as G

newtype Rocq ann = Rocq {get :: Doc ann}

-- to be migrated
class Keywords rep where
  import_ :: rep
  assign  :: rep
  univ    :: rep
  rec     :: rep
{-
  data_   :: rep
  arr     :: rep
-}

instance Keywords (Doc ann) where
  import_ = "Require" <+> "Import"
  assign  = ":="
  rec     = "Record"
  univ    = "Type"
{-
  data_   = "data"
  arr     = "->"
-}


class TypeAnn rep where
  typeAnn :: rep -> rep -> rep
  teleCell :: rep -> rep -> rep

instance TypeAnn (Doc ann) where
  typeAnn trm typ = trm <+> ":" <+> typ
  teleCell trm typ = parens $ trm <+> ":" <+> typ

arr, typedel :: String
arr = " -> "
typedel = " : "

-- FIXME: end '.' should not be hard-coded
printImport :: Import -> Doc ann
printImport (ImportLib VecMod) = import_ <+> "Coq.Vectors.Vector." <> hardline <>
  "Import VectorNotations." -- FIXME
printImport (ImportLib StringMod) = import_ <+> "Coq.Strings.String."
-- the rest are builtin
printImport (ImportLib NatMod) = emptyDoc
printImport (ImportLib ListMod) = emptyDoc

printType :: Type -> String
printType (Univ) = "Type" -- FIXME
printType (Con t) = if  "Cap_" `isPrefixOf` t ||
                        "Record" `isPrefixOf` t
                        then t else (map toLower t) -- if starts with keyword Cap_ maintain, else lower case
printType (Arr t1 t2) = printType t1 ++ arr ++ printType t2
printType (TVar t) = t
printType (PCon "Vec" args) = "t " ++ unwords (map printType args)
printType (PCon name types) = (map toLower name) ++ " " ++ unwords (map printType types)
printType (DCon name [] exprs) = name ++ " " ++ unwords (map printExpr exprs)
printType (DCon name types exprs) = name ++ " " ++ unwords (map printType types) ++ " " ++ unwords (map printExpr exprs)
printType (Index names ty) = "forall " ++ G.brackets (map toLower (unwords names) ++ typedel ++ printType ty)
printType (Embed e) = printExpr e

printReturnType :: Type -> String
printReturnType (Con t) = map toLower t --required for nested functions
printReturnType (Arr _ t) = printReturnType t
printReturnType _ = error "should not occur as a return type"

printArg :: Arg -> String
printArg a = G.parens $ (arg a) ++ typedel ++ (printType $ argty a)

printExpr :: Expr -> String
printExpr (Constructor name) = map toLower name
printExpr (Var var) = var
printExpr (Nat n) = show n
printExpr (String str) = G.quote str
printExpr (Paren e) = G.parens $ printExpr e
printExpr (Bin op e1 e2) = printExpr e1 ++ printOp op ++ printExpr e2
printExpr (Let ds expr) =
    foldl (\acc def -> acc ++ "let " ++ printLocalDefn def ++ " in\n    ") "" ds ++ printExpr expr --modified for proper let-in nesting
printExpr (If cond thn els) = "if " ++ printExpr cond ++ " then " ++ printExpr thn ++ " else " ++ printExpr els
printExpr (Where expr ds) = G.line (printExpr expr) ++ "\twhere " ++ foldl (\x y -> x ++ "\n\t" ++ y) "" (map printLocalDefn ds)
printExpr (FunCall fun args) = fun ++ " " ++ (unwords $ map printExpr args) -- Added case for FunCall
printExpr (VecE l) = G.sqbrackets $ intercalate "; " (map printExpr l)
printExpr (ListE l) = G.sqbrackets $ intercalate ", " (map printExpr l)
printExpr (Suc e) = G.parens $ "S " ++ printExpr e

printOp :: Op -> String
printOp Plus = " + "

printLocalDefn :: LocalDefn -> String
printLocalDefn (LocDefFun var Nothing args expr) = var ++ targs ++ " := " ++ printExpr expr -- FIXME
  where targs = if null args then "" else " " ++ intercalate " " (map printArg args)
printLocalDefn (LocDefFun var (Just t) args expr) = var ++ targs ++ typedel ++
  printReturnType t ++ " := " ++ printExpr expr -- FIXME
  where targs = if null args then "" else " " ++ intercalate " " (map printArg args)

printDef :: Definition -> Doc ann
printDef (DefTVar var Nothing expr) = pretty var <+> assign <+> (pretty . printExpr) expr
printDef (DefTVar var (Just t) expr) = 
  "Definition" <+> typeAnn (pretty var) (pretty $ printType t) <+> assign <+>
  (pretty $ printExpr expr) <> dot <> hardline
printDef (DefFun var Nothing args expr) = pretty var <+> (hsep $ map (pretty . arg) args) <+> assign <+> (pretty $ printExpr expr)
printDef (DefFun var (Just t) args expr) = "Definition" <+> pretty var <+>
  typeAnn (hsep $ map (pretty . printArg) args) (pretty $ printType t) <+> assign <+> (pretty $ printExpr expr) <> 
  "." <> hardline
printDef (DefPatt var params ty m cons) = "Fixpoint" <+> pretty var <+>
  typeAnn (hsep $ map (\(x, y) -> teleCell (pretty x) (pretty $ printType y)) params)
  -- (unwords $ map (\(x, y) -> " " ++ G.parens (x ++ typedel ++ printType y)) params) ++ 
          (pretty $ printType ty) <+> assign <> hardline <>
  "match" <+> pretty m <+> "with" <> hardline <>
  vsep (map (\(a, e) -> "|" <+> (hsep $ map (pretty . map toLower . arg) a) <+> "=>" <+> (pretty $ printExpr e)) cons) <>
  "end" <> dot <> hardline
   -- unwords (map (\(a, e) -> "\n| " ++ (unwords $ map (map toLower . arg) a) ++ " => " ++ printExpr e) cons) ++ " end."
printDef (DefDataType name args ty) = let
    printIndices :: Type -> Doc ann
    printIndices (Arr (Index n t) ctype) = (pretty $ printType (Index n t)) <> comma <+> (pretty $ printType ctype)
    printIndices t = pretty $ printType t
    in
        "Inductive" <+> typeAnn (pretty $ map toLower name) (pretty $ printType ty) <+>
        assign <> hardline <>
        (vsep (map (\(x, y) -> "|" <+> typeAnn (pretty $ map toLower x) (printIndices y)) args)) <> "."
printDef (DefPDataType name params args ty) = let
    printIndices :: Type -> Doc ann
    printIndices (Arr (Index n t) ctype) = (pretty $ printType (Index n t)) <> comma <+> (pretty $ printType ctype)
    printIndices t = pretty $ printType t
    in
        "Inductive" <+> (pretty $ map toLower name) <+>
         typeAnn (hsep (map (\(x, y) -> teleCell (pretty $ map toLower x) (pretty $ printType y)) params))
                 (pretty $ printType ty) <+> assign <> hardline <>
         vsep (map (\(x, y) -> "|" <+> typeAnn (pretty $ map toLower x) (printIndices y)) args) <> dot

--Function for Records
printDef (DefRecType name params consName fields _) =
    rec <+> typeAnn recName univ <+> assign <+> pretty consName <+>
    lbrace <> hardline <>
      indent 2 (vsep (map (\(fname, ftype) -> typeAnn (pretty fname) (pretty $ printType ftype) <> semi) fields))
      <> hardline <> rbrace <> dot <> hardline
    where
        recName = case params of
            [] -> pretty name
            _ -> pretty name <+> hsep (map (\(Arg n t) -> teleCell (pretty n) (pretty $ printType t)) params)

printDef (DefRec name recType consName fields) =
  "Definition" <+> typeAnn (pretty name) (pretty $ printType recType) <+> assign <> hardline <>
  indent 2 constructorCall <> dot <> hardline
  where
    hasParams = case recType of
      (DCon _ tys exps) -> Just (tys, exps)
      (Con _)           -> Nothing
      _                 -> error "invalid type for a record"
    parameters = maybe emptyDoc (\(tys, exps) -> (hsep $ map (pretty . printType) tys) <+> 
                                                 (hsep $ map (pretty . printExpr) exps)) hasParams
    -- Use the provided constructor name if available; otherwise, look up the default.

    fieldsStr = hsep $ map (pretty . printExpr . snd) fields

    -- If there are parameters, insert them between the constructor name and the field values.
    constructorCall = 
      case hasParams of
        Nothing -> pretty consName <+> fieldsStr
        Just _  -> pretty consName <+> parameters <+> fieldsStr

printDef (OpenName _) = emptyDoc
printDef (Separator c n b) =
  let s = pretty $ replicate (fromIntegral n) c in
  if b then hardline <> s <> hardline else s


printModule :: Module -> Rocq ann
printModule (Module name imports defs) =
    let headers =
          (if null imports then emptyDoc
                      else vsep (map printImport imports) <> hardline <> hardline) <>
            "Module" <+> pretty name <> dot <> hardline
        body = vcat (map printDef defs)
    in Rocq $ headers <> hardline <> body <> hardline <> "End" <+> pretty name <> dot

render :: Module -> String
render = renderString . layoutPretty defaultLayoutOptions . get . printModule

runRocq :: Module -> IO()
runRocq m = writeFile ("out/" ++ modname m ++ ".v") $ render m
