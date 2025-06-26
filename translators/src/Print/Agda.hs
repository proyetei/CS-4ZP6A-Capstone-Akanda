{-# Language OverloadedStrings #-}
module Print.Agda
  ( printModule
  , render
  , runAgda
  ) where

import Prettyprinter
import Prettyprinter.Render.String (renderString)

import Grammar

newtype Agda ann = Agda {get :: Doc ann}

-- to be migrated
class Keywords rep where
  import_ :: rep
  assign  :: rep
  rec     :: rep
  univ    :: rep
  data_   :: rep
  arr     :: rep
  lcons   :: rep

instance Keywords (Doc ann) where
  import_ = "open" <+> "import"
  assign  = "="
  rec     = "record"
  univ    = "Set"
  data_   = "data"
  arr     = "->"
  lcons   = "\x2237" 

class TypeAnn rep where
  typeAnn :: rep -> rep -> rep
  teleCell :: rep -> rep -> rep

instance TypeAnn (Doc ann) where
  typeAnn trm typ = trm <+> ":" <+> typ
  teleCell trm typ = parens $ trm <+> ":" <+> typ

printImport :: Import -> Doc ann
printImport (ImportLib NatMod) = import_ <+> "Agda.Builtin.Nat"
printImport (ImportLib VecMod) = import_ <+> "Data.Vec.Base"
printImport (ImportLib ListMod) = import_ <+> "Agda.Builtin.List"
printImport (ImportLib StringMod) = import_ <+> "Agda.Builtin.String"

-- Print types
printType :: Type -> Doc ann
printType (Univ) = univ
printType (Con t) = pretty t
printType (Arr t1 t2) = printType t1 <+> arr <+> printType t2
printType (TVar t) = pretty t
printType (PCon name types) = pretty name <+> hsep (map printType types)
printType (DCon name [] exprs) = -- For dependent type constructors
    pretty name <+> hsep (map printExpr exprs)
printType (DCon name types exprs) = -- For dependent type constructors
    pretty name <+> hsep (map printType types) <+> hsep (map printExpr exprs)
printType (Index names ty) = braces $ typeAnn (hsep $ map pretty names) (printType ty)
printType (Embed e) = printExpr e

printLit :: Literal -> Doc ann
printLit (Nat n) = pretty n
printLit (Bool b) = pretty b
printLit (String str) = dquotes $ pretty str
printLit (Vec l) = parens $ encloseSep emptyDoc (space <> lcons <+> lbracket <> rbracket)
  (space <> lcons <> space) (map printExpr l)
printLit (List l) = parens $ encloseSep emptyDoc (space <> lcons <+> lbracket <> rbracket)
  (space <> lcons <> space) (map printExpr l)

-- Print expressions
printExpr :: Expr -> Doc ann
printExpr (Constructor name) = pretty name
printExpr (Var var) = pretty var
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
printExpr (Suc t) = parens $ "suc" <+> printExpr t
printExpr (Lit l) = printLit l

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

-- Function to print variable definitions
printDef :: Definition -> Doc ann
printDef (DefTVar var Nothing expr) = 
  pretty var <+> assign <+> align (printExpr expr) <> softline'
printDef (DefTVar var (Just t) expr) = 
  typeAnn (pretty var) (printType t) <> hardline <>
  pretty var <+> assign <+> align (printExpr expr) <> hardline

-- Function to print function definitions
printDef (DefFun var ty args expr) = printLocalDefn (LocDefFun var ty args expr)

printDef (DefPatt var params ty _ cons) =
    typeAnn (pretty var) (printType (foldr Arr ty (map snd params))) <> line <>
    vsep (map (\(a, e) -> (pretty var) <+> hsep (map (pretty . arg) a) <+> assign <+> printExpr e) cons)
-- Function to print datatype definitions
printDef (DefDataType name cons ty) =
  data_ <+> typeAnn (pretty name) (printType ty) <+> "where" <> hardline <>
  indent 1 (vsep (map (\(n, t) -> typeAnn (pretty n) (printType t)) cons)) <>
   line
printDef (DefPDataType name params cons ty) =
  data_ <+> 
    typeAnn (pretty name <+> hsep (map (\(x, y) -> teleCell (pretty x) (printType y)) params))
              (printType ty) <+> 
    "where" <> hardline <>
    indent 1 (vsep (map (\(n,t) -> typeAnn (pretty n) (printType t)) cons)) <>
    hardline

-- Function for records
printDef (DefRecType name params consName fields _) =
    rec <+> typeAnn pp_params univ <+> "where" <> line <>
    indent 4 (vsep $ "constructor" <+> pretty consName : "field" : 
       (indent 4 $ vsep $ map (\(fname, ftype) -> typeAnn (pretty fname) (printType ftype)) fields)
       : []) <>
    hardline
    where
      ll = map (\(Arg n t) -> teleCell (pretty n) (printType t)) params
      pp_params = if null params then pretty name else pretty name <+> hsep ll

printDef (DefRec name recType consName fields) =
    typeAnn (pretty name) (printType recType) <> hardline <> 
    pretty name <+> assign <+> pretty consName <+> nest 4 (sep (map (printExpr . snd) fields))

printDef (OpenName _) = mempty
printDef (Separator '\n' n _) = vcat $ replicate (fromIntegral n) emptyDoc
printDef (Separator c n b) = 
  let s = hcat $ replicate (fromIntegral n) (pretty c) in
  if b then hardline <> s <> hardline else s


-- Print the Agda module
printModule :: Module -> Agda ann
printModule (Module name imports defs) =
    let
        headers = "module" <+> pretty name <+> "where" <> hardline <>
          vsep (map printImport imports)
        -- Concatenate all definitions
        body = vcat $ map printDef defs

    in Agda $ headers <> hardline <> hardline <> body

render :: Module -> String
render = renderString . layoutPretty defaultLayoutOptions . get . printModule

runAgda :: Module -> IO()
runAgda m = do
    writeFile ("out/" ++ name ++ ".agda") $ render m
    where name = modname m
