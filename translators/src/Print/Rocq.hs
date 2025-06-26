{-# Language OverloadedStrings #-}
module Print.Rocq
  ( printModule
  , render
  , runRocq
  ) where

import Data.Char (toLower)
import Data.List (isPrefixOf)

import Prettyprinter
import Prettyprinter.Render.String (renderString)

import Grammar
import Print.Generic (prettyArgs)

newtype Rocq ann = Rocq {get :: Doc ann}

-- to be migrated
class Keywords rep where
  import_ :: rep
  assign  :: rep
  univ    :: rep
  rec     :: rep
  arr     :: rep

instance Keywords (Doc ann) where
  import_ = "Require" <+> "Import"
  assign  = ":="
  rec     = "Record"
  univ    = "Type"
  arr     = "->"


class TypeAnn rep where
  typeAnn :: rep -> rep -> rep
  teleCell :: rep -> rep -> rep

instance TypeAnn (Doc ann) where
  typeAnn trm typ = trm <+> ":" <+> typ
  teleCell trm typ = parens $ trm <+> ":" <+> typ


-- FIXME: end '.' should not be hard-coded
printImport :: Import -> Doc ann
printImport (ImportLib VecMod) = import_ <+> "Coq.Vectors.Vector." <> hardline <>
  "Import VectorNotations." -- FIXME
printImport (ImportLib StringMod) = import_ <+> "Coq.Strings.String."
-- the rest are builtin
printImport (ImportLib NatMod) = emptyDoc
printImport (ImportLib ListMod) = emptyDoc

printType :: Type -> Doc ann
printType (Univ) = univ
printType (Con t) = pretty $ if  "Cap_" `isPrefixOf` t || "Record" `isPrefixOf` t
                             then t else (map toLower t) -- if starts with keyword Cap_ maintain, else lower case
printType (Arr t1 t2) = printType t1 <+> arr <+> printType t2
printType (TVar t) = pretty t
printType (PCon "Vec" args) = "Vect" <+> hsep (map printType args)
printType (PCon name types) = pretty (map toLower name) <+> hsep (map printType types)
printType (DCon name [] exprs) = pretty name <+> hsep (map printExpr exprs)
printType (DCon name types exprs) = pretty name <+> hsep (map printType types) <+> hsep (map printExpr exprs)
printType (Index names ty) = "forall" <+> brackets (typeAnn (pretty $ map toLower (unwords names)) (printType ty))
printType (Embed e) = printExpr e

printReturnType :: Type -> Doc ann
printReturnType (Con t) = pretty $ map toLower t --required for nested functions
printReturnType (Arr _ t) = printReturnType t
printReturnType _ = error "should not occur as a return type"

printArg :: Arg -> Doc ann
printArg a = parens $ typeAnn (pretty $ arg a) (printType $ argty a)

printLit :: Literal -> Doc ann
printLit (Nat n) = pretty n
printLit (Bool b) = pretty b
printLit (String str) = dquotes $ pretty str
printLit (Vec l) = encloseSep lbracket rbracket (semi <> space) (map printExpr l)
printLit (List l) = encloseSep lbracket rbracket (comma <> space) (map printExpr l)

printExpr :: Expr -> Doc ann
printExpr (Constructor name) = pretty $ map toLower name
printExpr (Var var) = pretty var
printExpr (Paren e) = parens $ printExpr e
printExpr (Bin op e1 e2) = printExpr e1 <+> printOp op <+> printExpr e2
printExpr (Let ds expr) =
  "let" <+> align (vcat (map printLocalDefn ds) <+> "in") <> line <>
  printExpr expr
printExpr (If cond thn els) = "if" <+> printExpr cond <+> "then" <+> printExpr thn <+> "else" <+> printExpr els
printExpr (Where expr ds) = 
  printExpr expr <> hardline <>
  indent 4 ("where " <+> vcat (map printLocalDefn ds))
printExpr (FunCall fun args) = pretty fun <+> (hsep $ map printExpr args)
printExpr (Suc e) = parens $ "S" <+> printExpr e
printExpr (Lit l) = printLit l

printOp :: Op -> Doc ann
printOp Plus = "+"

printLocalDefn :: LocalDefn -> Doc ann
printLocalDefn (LocDefFun var Nothing args expr) = 
  prettyArgs var printArg args <+> assign <+> printExpr expr
printLocalDefn (LocDefFun var (Just t) args expr) = typeAnn targs (printReturnType t) <+> assign <+>
  printExpr expr
  where targs = prettyArgs var printArg args

printDef :: Definition -> Doc ann
printDef (DefTVar var Nothing expr) = pretty var <+> assign <+> printExpr expr
printDef (DefTVar var (Just t) expr) = 
  nest 4 ("Definition" <+> typeAnn (pretty var) (printType t) <+> assign <> softline <>
  (printExpr expr <> dot <> hardline))
printDef (DefFun var Nothing args expr) = pretty var <+> (hsep $ map (pretty . arg) args) <+> assign <+> printExpr expr
printDef (DefFun var (Just t) args expr) = "Definition" <+> pretty var <+>
  typeAnn (hsep $ map printArg args) (printType t) <+> assign <> softline' <> printExpr expr <> 
  "." <> hardline
printDef (DefPatt var params ty m cons) = "Fixpoint" <+> pretty var <+>
  typeAnn (hsep $ map (\(x, y) -> teleCell (pretty x) (printType y)) params)
          (printType ty) <+> assign <> hardline <>
  "match" <+> pretty m <+> "with" <> hardline <>
  vsep (map (\(a, e) -> pipe <+> (hsep $ map (pretty . map toLower . arg) a) <+> "=>" <+> printExpr e) cons) 
  <> softline' <> "end" <> dot <> hardline
printDef (DefDataType name args ty) = let
    printIndices :: Type -> Doc ann
    printIndices (Arr (Index n t) ctype) = printType (Index n t) <> comma <+> printType ctype
    printIndices t = printType t
    in
        "Inductive" <+> typeAnn (pretty $ map toLower name) (printType ty) <+>
        assign <> hardline <>
        (vsep (map (\(x, y) -> pipe <+> typeAnn (pretty $ map toLower x) (printIndices y)) args)) <> "."
printDef (DefPDataType name params args ty) = let
    printIndices :: Type -> Doc ann
    printIndices (Arr (Index n t) ctype) = (printType (Index n t)) <> comma <+> (printType ctype)
    printIndices t = printType t
    in
        "Inductive" <+> (pretty $ map toLower name) <+>
         typeAnn (hsep (map (\(x, y) -> teleCell (pretty $ map toLower x) (printType y)) params))
                 (printType ty) <+> assign <> hardline <>
         vsep (map (\(x, y) -> pipe <+> typeAnn (pretty $ map toLower x) (printIndices y)) args) <> dot

--Function for Records
printDef (DefRecType name params consName fields _) =
    rec <+> typeAnn recName univ <+> assign <+> pretty consName <+>
    lbrace <> hardline <>
      indent 2 (vsep (map (\(fname, ftype) -> typeAnn (pretty fname) (printType ftype) <> semi) fields))
      <> hardline <> rbrace <> dot <> hardline
    where
        recName = case params of
            [] -> pretty name
            _ -> pretty name <+> hsep (map (\(Arg n t) -> teleCell (pretty n) (printType t)) params)

printDef (DefRec name recType consName fields) =
  "Definition" <+> typeAnn (pretty name) (printType recType) <+> assign <> hardline <>
  indent 2 constructorCall <> dot <> hardline
  where
    hasParams = case recType of
      (DCon _ tys exps) -> Just (tys, exps)
      (Con _)           -> Nothing
      _                 -> error "invalid type for a record"
    parameters = maybe emptyDoc (\(tys, exps) -> (hsep $ map printType tys) <+> 
                                                 (hsep $ map printExpr exps)) hasParams
    -- Use the provided constructor name if available; otherwise, look up the default.

    fieldsStr = hsep $ map (printExpr . snd) fields

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
