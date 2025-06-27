module Print.Agda
  ( printModule
  , render
  , runAgda
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as T (writeFile)
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)

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

-- Print Terms
printTm :: Tm -> Doc ann
printTm (Univ) = univ
printTm (Arr t1 t2) = printTm t1 <+> arr <+> printTm t2
printTm (PCon t []) = pretty t
printTm (PCon name types) = pretty name <+> hsep (map printTm types)
printTm (DCon name types) = pretty name <+> hsep (map printTm types)
printTm (Index names ty) = braces $ typeAnn (hsep $ map pretty names) (printTm ty)
printTm (Var var) = pretty var
printTm (Paren e) = parens $ printTm e
printTm (Binary op e1 e2) = printTm e1 <+> printOp2 op <+> printTm e2
printTm (Let ds expr) = 
  "let" <+> align (vcat (map printLocalDefn ds) <+> "in") <> line <>
  printTm expr
printTm (If cond thn els) =
  "if" <+> printTm cond <+> "then" <+> printTm thn <+> "else" <+> printTm els
printTm (Where expr ds) =
  printTm expr <> line <>
  indent 4 ("where" <> vcat (map printLocalDefn ds))
printTm (App fun args) = printTm fun <+> softline' <> (sep $ map printTm args)
printTm (Unary o t) = parens $ printOp1 o <+> printTm t
printTm (Lit l) = printLit l

printOp1 :: Op1 -> Doc ann
printOp1 Suc = "suc"

printOp2 :: Op2 -> Doc ann
printOp2 Plus = "+"

printLit :: Literal -> Doc ann
printLit (Nat n) = pretty n
printLit (Bool b) = pretty b
printLit (String str) = dquotes $ pretty str
printLit (Vec l) = parens $ encloseSep emptyDoc (space <> lcons <+> lbracket <> rbracket)
  (space <> lcons <> space) (map printTm l)
printLit (List l) = parens $ encloseSep emptyDoc (space <> lcons <+> lbracket <> rbracket)
  (space <> lcons <> space) (map printTm l)

printLocalDefn :: LocalDefn -> Doc ann
printLocalDefn (LocDefFun var ty args expr) =
   typeSig <> tvar <+> assign <+> align (printTm expr)
    where
        typeSig = case ty of
            Just t -> typeAnn (pretty var) (printTm t) <> line
            Nothing -> mempty
        tvar = case args of
            [] -> pretty var
            (_:_) -> pretty var <+> (hsep $ map (pretty . arg) args)

-- Function to print variable definitions
printDef :: Definition -> Doc ann
printDef (DefTVar var Nothing expr) = 
  pretty var <+> assign <+> align (printTm expr) <> softline'
printDef (DefTVar var (Just t) expr) = 
  typeAnn (pretty var) (printTm t) <> hardline <>
  pretty var <+> assign <+> align (printTm expr) <> hardline

printDef (DefPatt var params ty _ cons) =
    typeAnn (pretty var) (printTm (foldr Arr ty (map snd params))) <> line <>
    vsep (map (\(a, e) -> (pretty var) <+> hsep (map (pretty . arg) a) <+> assign <+> printTm e) cons)
-- Function to print datatype definitions
printDef (DefDataType name cons ty) =
  data_ <+> typeAnn (pretty name) (printTm ty) <+> "where" <> hardline <>
  indent 1 (vsep (map (\(n, t) -> typeAnn (pretty n) (printTm t)) cons)) <>
   line
printDef (DefPDataType name params cons ty) =
  data_ <+> 
    typeAnn (pretty name <+> hsep (map (\(x, y) -> teleCell (pretty x) (printTm y)) params))
              (printTm ty) <+> 
    "where" <> hardline <>
    indent 1 (vsep (map (\(n,t) -> typeAnn (pretty n) (printTm t)) cons)) <>
    hardline

-- Function for records
printDef (DefRecType name params consName fields _) =
    rec <+> typeAnn pp_params univ <+> "where" <> line <>
    indent 4 (vsep $ "constructor" <+> pretty consName : "field" : 
       (indent 4 $ vsep $ map (\(fname, ftype) -> typeAnn (pretty fname) (printTm ftype)) fields)
       : []) <>
    hardline
    where
      ll = map (\(Arg n t) -> teleCell (pretty n) (printTm t)) params
      pp_params = if null params then pretty name else pretty name <+> hsep ll

printDef (DefRec name recTm consName fields) =
    typeAnn (pretty name) (printTm recTm) <> hardline <> 
    pretty name <+> assign <+> pretty consName <+> nest 4 (sep (map (printTm . snd) fields))

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

render :: Module -> T.Text
render = renderStrict . layoutPretty defaultLayoutOptions . get . printModule

runAgda :: Module -> IO()
runAgda m = do
    T.writeFile ("out/" ++ (T.unpack $ modname m) ++ ".agda") $ render m
