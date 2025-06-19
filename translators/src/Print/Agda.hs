{-# Language OverloadedStrings #-}
module Print.Agda
  ( printModule
  , get
  , runAgda
  ) where

import Prettyprinter

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
  lcons   = " ∷ " 

class TypeAnnot rep where
  typeannot :: rep -> rep -> rep
  ptypeannot :: rep -> rep -> rep

instance TypeAnnot (Doc ann) where
  typeannot trm typ = trm <+> ":" <+> typ
  ptypeannot trm typ = parens $ trm <+> ":" <+> typ

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
printType (Index names ty) = braces $ typeannot (hsep $ map pretty names) (printType ty)
printType (Embed e) = printExpr e

-- Print expressions
printExpr :: Expr -> Doc ann
printExpr (Constructor name) = pretty name
printExpr (Var var) = pretty var
printExpr (Nat n) = pretty n
printExpr (String str) = dquotes $ pretty str
printExpr (Paren e) = parens $ printExpr e
printExpr (Bin op e1 e2) = printExpr e1 <+> printOp op <+> printExpr e2
printExpr (Let ds expr) = 
  "let" <+> align (vcat (map printDef ds) <+> "in") <> line <>
  printExpr expr
  -- "let\n    " ++ (intercalate "\n    " (map (show . printDef) ds)) ++ " in\n    " ++ printExpr expr
printExpr (If cond thn els) =
  "if" <+> printExpr cond <+> "then" <+> printExpr thn <+> "else" <+> printExpr els
printExpr (Where expr ds) =
  printExpr expr <> line <>
  indent 4 ("where" <> vcat (map printDef ds))
printExpr (FunCall fun args) = pretty fun <+> (fillSep (map (group . printExpr) args))
printExpr (VecE l) = align $ encloseSep (lparen <> lbracket) (rbracket <> rparen) lcons (map printExpr l)
printExpr (ListE l) = encloseSep (lparen <> lbracket) (rbracket <> rparen) lcons (map (group . printExpr) l)
printExpr (Suc t) = parens $ "suc" <+> printExpr t

printOp :: Op -> Doc ann
printOp Plus = "+"

-- Function to print variable definitions
printDef :: Definition -> Doc ann
printDef (DefTVar var t expr) = 
  typeannot (pretty var) (printType t) <> line <>
  pretty var <+> assign <+> align (printExpr expr) <> line
printDef (DefUVar var expr) = pretty var <+> assign <+> align (printExpr expr) <> softline

-- Function to print function definitions
printDef (DefFun var ty args expr) = 
   typeSig <> pretty var <+> argsStr <+> assign <+> align (printExpr expr)
    where
        typeSig = case ty of
            Just t -> typeannot (pretty var) (printType t) <> line
            Nothing -> mempty
        argsStr = hsep $ map (pretty . arg) args

printDef (DefNesFun var Nothing args expr) = printDef (DefFun var Nothing args expr)
printDef (DefNesFun var (Just t) args expr) = printDef (DefFun var (Just t) args expr)
printDef (DefPatt var params ty _ cons) =
    typeannot (pretty var) (printType (foldr Arr ty (map snd params))) <> line <>
    vsep (map (\(a, e) -> (pretty var) <+> hsep (map (pretty . arg) a) <+> assign <+> printExpr e) cons)
-- Function to print datatype definitions
printDef (DefDataType name cons ty) =
  data_ <+> typeannot (pretty name) (printType ty) <+> "where" <> line <>
  indent 1 (vsep (map (\(n, t) -> typeannot (pretty n) (printType t)) cons)) <>
   line
printDef (DefPDataType name params cons ty) =
  data_ <+> 
    typeannot (pretty name <+> hsep (map (\(x, y) -> ptypeannot (pretty x) (printType y)) params))
              (printType ty) <+> 
    "where" <> line <>
    indent 1 (vsep (map (\(n,t) -> typeannot (pretty n) (printType t)) cons)) <>
    line

-- Function for records
printDef (DefRecType name params consName fields _) =
    rec <+> typeannot pp_params univ <+> "where" <> line <>
    indent 4 (vsep $ "constructor" <+> pretty consName : "field" : 
       (indent 4 $ vsep $ map (\(fname, ftype) -> typeannot (pretty fname) (printType ftype)) fields)
       : []) <>
    line
    where
      ll = map (\(Arg n t) -> ptypeannot (pretty n) (printType t)) params
      pp_params = if null params then pretty name else pretty name <+> hsep ll

printDef (DefRec name recType consName fields) =
    typeannot (pretty name) (printType recType) <> line <> 
    pretty name <+> assign <+> pretty consName <+> nest 4 (sep (map (printExpr . snd) fields))

printDef (OpenName _) = mempty
printDef (Separator '\n' n _) = vcat $ replicate (fromIntegral n) emptyDoc
printDef (Separator c n b) = 
  let s = hcat $ replicate (fromIntegral n) (pretty c) in
  if b then line <> s <> line else s


-- Print the Agda module
printModule :: Module -> Agda ann
printModule (Module name imports defs) =
    let
        headers = "module" <+> pretty name <+> "where" <>
          line <> vsep (map printImport imports)
        -- Concatenate all definitions
        body = vcat $ map printDef defs

    in Agda $ headers <> line <> line <> body

runAgda :: Module -> IO()
runAgda m = do
    writeFile ("out/" ++ name ++ ".agda") $ show $ get $ printModule m
    where name = modname m
