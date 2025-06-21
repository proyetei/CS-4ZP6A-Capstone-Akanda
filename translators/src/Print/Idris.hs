{-# Language OverloadedStrings #-}
module Print.Idris
  ( printModule
  , render
  , runIdris
  ) where

import Prettyprinter
import Prettyprinter.Render.String (renderString)

import Grammar

newtype Idris ann = Idris {get :: Doc ann}

-- to be migrated
class Keywords rep where
  import_ :: rep
  assign  :: rep
  data_   :: rep
  rec     :: rep
  univ    :: rep
  arr     :: rep

instance Keywords (Doc ann) where
  import_ = "import"
  assign  = "="
  data_   = "data"
  rec     = "record"
  univ    = "Type"
  arr     = "->"

class TypeAnn rep where
  typeAnn :: rep -> rep -> rep
  teleCell :: rep -> rep -> rep

instance TypeAnn (Doc ann) where
  typeAnn trm typ = trm <+> ":" <+> typ
  teleCell trm typ = parens $ trm <+> ":" <+> typ

printImport :: Import -> Doc ann
printImport (ImportLib VecMod) = import_ <+> "Data.Vect"
-- There rest are builtin
printImport (ImportLib NatMod) = mempty
printImport (ImportLib StringMod) = mempty
printImport (ImportLib ListMod) = mempty

printType :: Type -> Doc ann
printType (Univ) = univ
printType (Con t) = pretty t
printType (Arr t1 t2) = printType t1 <+> arr <+> printType t2
printType (TVar t) = pretty t
printType (PCon "Vec" [Con baseType, size]) = "Vect" <+> printType size <+> pretty baseType
printType (PCon name types) = pretty name <+> hsep (map printType types)
printType (DCon name [] exprs) = pretty name <+> hsep (map printExpr exprs)
printType (DCon name types exprs) = -- For dependent type constructors
    pretty name <+> hsep (map printType types) <+> hsep (map printExpr exprs)
printType (Index names ty) = braces $ typeAnn (hsep $ punctuate comma $ map pretty names) (printType ty)
printType (Embed e) = printExpr e


printExpr :: Expr -> Doc ann
printExpr (Constructor name) = pretty name
printExpr (Var var) = pretty var
printExpr (Nat n) = pretty n
printExpr (String str) = dquotes $ pretty str
printExpr (Paren e) = parens $ printExpr e
printExpr (Bin op e1 e2) = printExpr e1 <+> printOp op <+> printExpr e2
printExpr (Let ds expr) = 
  "let" <+> align (vcat (map printLocalDefn ds) <+> "in") <> line <>
  printExpr expr
printExpr (If cond thn els) =
  "if" <+> printExpr cond <+> "then" <+> printExpr thn <+> "else" <+> printExpr els
printExpr (Where expr ds) =
  printExpr expr <> line <>
  indent 4 ("where" <> vcat (map printLocalDefn ds))
printExpr (FunCall fun args) = pretty fun <+> (fillSep (map (group . printExpr) args))
printExpr (VecE l) = encloseSep lbracket rbracket (comma <> space) (map printExpr l)
printExpr (ListE l) = encloseSep lbracket rbracket (comma <> space) (map printExpr l)
printExpr (Suc t) = parens $ "S" <+> printExpr t

printOp :: Op -> Doc ann
printOp Plus = "+"

printLocalDefn :: LocalDefn -> Doc ann
printLocalDefn (LocDefFun var ty args expr) = 
  typeSig <> tvar <+> assign <+> align (printExpr expr)
    where
        typeSig = case ty of
            Just t -> typeAnn (pretty var) (printType t) <> line
            Nothing -> mempty
        tvar = case args of
            [] -> pretty var
            (_:_) -> pretty var <+> (hsep $ map (pretty . arg) args)

printDef :: Definition -> Doc ann
printDef (DefTVar var Nothing expr) = 
  pretty var <+> assign <+> align (printExpr expr) <> softline'
printDef (DefTVar var (Just t) expr) = 
  typeAnn (pretty var) (printType t) <> hardline <>
  pretty var <+> assign <+> align (printExpr expr) <> hardline

printDef (DefFun var ty args expr) = printLocalDefn (LocDefFun var ty args expr)
printDef (DefPatt var params ty _ cons) =
    typeAnn (pretty var) (printType (foldr Arr ty (map snd params))) <> line <>
    vsep (map (\(a, e) -> (pretty var) <+> hsep (map (pretty . arg) a) <+> assign <+> printExpr e) cons)
printDef (DefDataType name cons ty) =
  data_ <+> typeAnn (pretty name) (printType ty) <+> "where" <> hardline <>
  indent 1 (vsep (map (\(n, t) -> typeAnn (pretty n) (printType t)) cons)) <>
   hardline
printDef (DefPDataType name params cons ty) =
  data_ <+> 
    typeAnn (pretty name) (concatWith (\x y -> x <+> arr <+> y) (map (\(x, y) -> teleCell (pretty x) (printType y)) params)) <+> arr <+> (printType ty) <+> 
    "where" <> hardline <>
    indent 1 (vsep (map (\(n,t) -> typeAnn (pretty n) 
                                   (encloseSep emptyDoc (space <> arr) (space <> arr <> space) (map (pretty.fst) params)) <+> printType t) cons)) <>
    hardline

printDef (DefRecType name params consName fields _) =
    rec <+> pp_params <+> "where" <> hardline <>
    indent 4 (vsep $ "constructor" <+> pretty consName :
       (vsep $ map (\(fname, ftype) -> typeAnn (pretty fname) (printType ftype)) fields)
       : []) <>
    hardline
    where
      ll = map (\(Arg n t) -> teleCell (pretty n) (printType t)) params
      pp_params = if null params then pretty name else pretty name <+> hsep ll

printDef (DefRec name recType consName fields) =
    typeAnn (pretty name) (printType recType) <> hardline <> 
    pretty name <+> assign <+> pretty consName <+> nest 4 (sep (map (printExpr . snd) fields)) <>
    hardline

printDef (OpenName _) = emptyDoc
printDef (Separator '\n' n _) = vcat $ replicate (fromIntegral n) emptyDoc
printDef (Separator c n b) = 
  let s = hcat $ replicate (fromIntegral n) (pretty c) in
  if b then hardline <> s <> hardline else s

printModule :: Module -> Idris ann
printModule (Module _ imports defs) =
    let
        headers = "module Main" <> hardline <>
          (if null imports then mempty else vsep (map printImport imports) <> hardline)
        -- Concatenate all definitions
        body = vcat $ map printDef defs

    in Idris $ headers <> hardline <> body <> hardline <>
               "main : IO()" <> hardline <>
               "main = putStrLn " <> dquote <> dquote

render :: Module -> String
render = renderString . layoutPretty defaultLayoutOptions . get . printModule

runIdris :: Module -> IO()
runIdris m = writeFile ("out/" ++ modname m ++ ".idr") $ render m
