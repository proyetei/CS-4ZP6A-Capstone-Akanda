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

printTm :: Tm -> Doc ann
printTm (Univ) = univ
printTm (Arr t1 t2) = printTm t1 <+> arr <+> printTm t2
printTm (PCon t []) = pretty t
printTm (PCon "Vec" [PCon baseType [], size]) = "Vect" <+> printTm size <+> pretty baseType
printTm (PCon name types) = pretty name <+> hsep (map printTm types)
printTm (DCon name types) = pretty name <+> hsep (map printTm types)
printTm (Index names ty) = braces $ typeAnn (hsep $ punctuate comma $ map pretty names) (printTm ty)
printTm (Constructor name) = pretty name
printTm (Var var) = pretty var
printTm (Paren e) = parens $ printTm e
printTm (Binary op e1 e2) = printTm e1 <+> printOp2 op <+> printTm e2
printTm (Let ds expr) = 
  "let" <+> align (vcat (map printLocalDefn ds) <+> "in") <> line <>
  printTm expr
printTm (If cond thn els) =
  "if" <+> printTm cond <+> "then" <+> printTm thn <+> "else" <+> printTm els
printTm (Where expr ds) =
  printTm expr <> hardline <>
  indent 4 ("where" <> vcat (map printLocalDefn ds))
printTm (App fun args) = printTm fun <+> (fillSep (NE.toList $ NE.map (group . printTm) args))
printTm (Unary o t) = parens $ printOp1 o <+> printTm t
printTm (Lit l) = printLit l

printLit :: Literal -> Doc ann
printLit (Nat n) = pretty n
printLit (Bool b) = pretty b
printLit (String str) = dquotes $ pretty str
printLit (Vec l) =  brackets $ fillSep $ punctuate comma $ map printTm l
printLit (List l) = brackets $ fillSep $ punctuate comma $ map printTm l

printOp1 :: Op1 -> Doc ann
printOp1 Suc = "S"

printOp2 :: Op2 -> Doc ann
printOp2 Plus = "+"

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

printDef :: Definition -> Doc ann
printDef (DefTVar var Nothing expr) = 
  pretty var <+> assign <+> align (printTm expr) <> softline'
printDef (DefTVar var (Just t) expr) = 
  typeAnn (pretty var) (printTm t) <> hardline <>
  pretty var <+> assign <+> align (printTm expr) <> hardline

printDef (DefPatt var params ty _ cons) =
    typeAnn (pretty var) (printTm (foldr Arr ty (map snd params))) <> line <>
    vsep (map (\(a, e) -> (pretty var) <+> hsep (map (pretty . arg) a) <+> assign <+> printTm e) cons)
printDef (DefDataType name cons ty) =
  data_ <+> typeAnn (pretty name) (printTm ty) <+> "where" <> hardline <>
  indent 1 (vsep (map (\(n, t) -> typeAnn (pretty n) (printTm t)) cons)) <>
   hardline
printDef (DefPDataType name params cons ty) =
  data_ <+> 
    typeAnn (pretty name) prettyParams <+> arr <+> (printTm ty) <+> 
    "where" <> hardline <> indent 1 (vsep (map prettyCon cons)) <> hardline
    where
      -- FIXME: do we really need this many arrows in the definitions?
      prettyParams = concatWith (\x y -> x <+> arr <+> y) $
                                map (\(x, y) -> teleCell (pretty x) (printTm y)) params
      prettyCon (n, t) = typeAnn (pretty n) 
                                 (encloseSep emptyDoc (space <> arr) (space <> arr <> space) 
                                      (map (pretty.fst) params)) <+> 
                         printTm t

printDef (DefRecType name params consName fields _) =
    rec <+> pp_params <+> "where" <> hardline <>
    indent 4 (vsep $ "constructor" <+> pretty consName :
       (vsep $ map (\(fname, ftype) -> typeAnn (pretty fname) (printTm ftype)) fields)
       : []) <>
    hardline
    where
      ll = map (\(Arg n t) -> teleCell (pretty n) (printTm t)) params
      pp_params = if null params then pretty name else pretty name <+> hsep ll

printDef (DefRec name recType consName fields) =
    typeAnn (pretty name) (printTm recType) <> hardline <> 
    pretty name <+> assign <+> pretty consName <+> nest 4 (sep (map (printTm . snd) fields)) <>
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
