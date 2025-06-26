{-# Language OverloadedStrings #-}
module Print.Idris
  ( printModule
  , render
  , runIdris
  ) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Prettyprinter
import Prettyprinter.Render.String (renderString)

import Grammar
import Print.Generic (blanklines)

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

-- append an Import if needed
printWithImport :: Import -> Doc ann -> Doc ann
printWithImport (ImportLib VecMod) m = m <> import_ <+> "Data.Vect" <> hardline
-- There rest are builtin
printWithImport (ImportLib NatMod) m = m
printWithImport (ImportLib StringMod) m = m
printWithImport (ImportLib ListMod) m = m

printType :: Type -> Doc ann
printType (Univ) = univ
printType (Arr t1 t2) = printType t1 <+> arr <+> printType t2
printType (TVar t) = pretty t
printType (PCon t []) = pretty t
printType (PCon "Vec" [PCon baseType [], size]) = "Vect" <+> printType size <+> pretty baseType
printType (PCon name types) = pretty name <+> hsep (map printType types)
printType (DCon name [] exprs) = pretty name <+> hsep (map printExpr exprs)
printType (DCon name types exprs) = -- For dependent type constructors
    pretty name <+> hsep (map printType types) <+> hsep (map printExpr exprs)
printType (Index names ty) = braces $ typeAnn (hsep $ punctuate comma $ map pretty names) (printType ty)
printType (Embed e) = printExpr e


printLit :: Literal -> Doc ann
printLit (Nat n) = pretty n
printLit (Bool b) = pretty b
printLit (String str) = dquotes $ pretty str
printLit (Vec l) =  brackets $ fillSep $ punctuate comma $ map printExpr l
printLit (List l) = brackets $ fillSep $ punctuate comma $ map printExpr l

printExpr :: Expr -> Doc ann
printExpr (Constructor name) = pretty name
printExpr (Var var) = pretty var
printExpr (Paren e) = parens $ printExpr e
printExpr (Binary op e1 e2) = printExpr e1 <+> printOp2 op <+> printExpr e2
printExpr (Let ds expr) = 
  "let" <+> align (vcat (map printLocalDefn ds) <+> "in") <> line <>
  printExpr expr
printExpr (If cond thn els) =
  "if" <+> printExpr cond <+> "then" <+> printExpr thn <+> "else" <+> printExpr els
printExpr (Where expr ds) =
  printExpr expr <> hardline <>
  indent 4 ("where" <> vcat (map printLocalDefn ds))
printExpr (App fun args) = printExpr fun <+> (fillSep (NE.toList $ NE.map (group . printExpr) args))
printExpr (Unary o t) = parens $ printOp1 o <+> printExpr t
printExpr (Lit l) = printLit l

printOp1 :: Op1 -> Doc ann
printOp1 Suc = "S"

printOp2 :: Op2 -> Doc ann
printOp2 Plus = "+"

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
    typeAnn (pretty name) prettyParams <+> arr <+> (printType ty) <+> 
    "where" <> hardline <> indent 1 (vsep (map prettyCon cons)) <> hardline
    where
      -- FIXME: do we really need this many arrows in the definitions?
      prettyParams = concatWith (\x y -> x <+> arr <+> y) $
                                map (\(x, y) -> teleCell (pretty x) (printType y)) params
      prettyCon (n, t) = typeAnn (pretty n) 
                                 (encloseSep emptyDoc (space <> arr) (space <> arr <> space) 
                                      (map (pretty.fst) params)) <+> 
                         printType t

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
printDef (Separator '\n' n _) = blanklines n
printDef (Separator c n b) = 
  let s = hcat $ replicate (fromIntegral n) (pretty c) in
  if b then hardline <> s <> hardline else s

printModule :: Module -> Idris ann
printModule (Module _ imports defs) =
    let
        mmain = "module Main" <> hardline
        headers = foldr printWithImport mmain imports
        -- Concatenate all definitions
        body = vcat $ map printDef defs

    in Idris $ headers <> hardline <> body <> hardline <>
               "main : IO()" <> hardline <>
               "main = putStrLn " <> dquote <> dquote

render :: Module -> String
render = renderString . layoutPretty defaultLayoutOptions . get . printModule

runIdris :: Module -> IO()
runIdris m = writeFile ("out/" ++ (T.unpack $ modname m) ++ ".idr") $ render m
