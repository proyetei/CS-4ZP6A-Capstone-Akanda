{-# Language OverloadedStrings #-}
module Print.Lean
  ( printModule
  , runLean
  , render
  ) where

import Prettyprinter
import Prettyprinter.Render.String (renderString)

import Grammar
import Print.Generic

newtype Lean ann = Lean {get :: Doc ann}

class Keywords rep where
  import_ :: rep
  assign  :: rep
  arr     :: rep
  univ    :: rep
{-
  data_   :: rep
  rec     :: rep
-}
instance Keywords (Doc ann) where
  import_ = "import"
  assign  = ":="
  arr     = "->"
  univ    = "Type"
{-
  data_   = "data"
  rec     = "record"
-}

class TypeAnn rep where
  typeAnn :: rep -> rep -> rep
  teleCell :: rep -> rep -> rep

instance TypeAnn (Doc ann) where
  typeAnn trm typ = trm <+> ":" <+> typ
  teleCell trm typ = parens $ trm <+> ":" <+> typ

-- append an Import if needed
printWithImport :: Import -> Doc ann -> Doc ann
printWithImport (ImportLib VecMod) m = m <> import_ <+> "Init.Data.Vector" <> hardline
-- There rest are builtin
printWithImport (ImportLib NatMod) m = m
printWithImport (ImportLib StringMod) m = m
printWithImport (ImportLib ListMod) m = m

-- Print types
printType :: Type -> Doc ann
printType (Univ) = univ
printType (Arr t1 t2) = printType t1 <+> arr <+> printType t2
printType (TVar t) = pretty t
printType (PCon t []) = pretty t
printType (PCon "Vec" args) = "Vector" <+> hsep (map printType args)
printType (PCon name types) = pretty name <+> hsep (map printType types)
printType (DCon name [] exprs) = pretty name <+> hsep (map printExpr exprs)
printType (DCon name types exprs) = pretty name <+> hsep (map printType types) <+> hsep (map printExpr exprs)
printType (Index names ty) = braces $ typeAnn (hsep (map pretty names)) (printType ty)
printType (Embed e) = printExpr e

printReturnType :: Type -> Doc ann
printReturnType (PCon t []) = pretty t
printReturnType (Arr _ t) = printReturnType t
printReturnType _ = error "show not occur as a return type"

printArg :: Arg -> Doc ann
printArg a = teleCell (pretty $ arg a) (printType $ argty a)

printLit :: Literal -> Doc ann
printLit (Nat n) = pretty n
printLit (Bool b) = pretty b
printLit (String str) = dquotes $ pretty str
printLit (Vec l) = "#" <> brackets (hsep $ punctuate comma (map printExpr l))
printLit (List l) = brackets $ hsep $ punctuate comma (map printExpr l)

-- Print expressions (unchanged)
printExpr :: Expr -> Doc ann
printExpr (Constructor name) = pretty name
printExpr (Var var) = pretty var
printExpr (Paren e) = parens $ printExpr e
printExpr (Binary op e1 e2) = printExpr e1 <+> printOp2 op <+> printExpr e2
printExpr (Let [] expr) = printExpr expr
printExpr (Let (d:[]) expr) = "let" <+> printLocalDefn d <> hardline <> printExpr expr
printExpr (Let (d:ds) expr) = 
  -- "let" <+> intercalate "\nlet " (map printLocalDefn (d:ds)) ++ "\n" ++ printExpr expr
  vcat (map (\x -> "let" <+> printLocalDefn x) (d:ds)) <> line <>
  printExpr expr
printExpr (If cond thn els) = "if" <+> printExpr cond <+> "then" <> hardline <> 
  indent 4 (printExpr thn) <> hardline <> 
  "else" <+> printExpr els
printExpr (Where expr ds) = printExpr expr <> hardline <>
  indent 4 ("where" <> hardline <> vsep (map printLocalDefn ds))
printExpr (FunCall fun args) = pretty fun <+> hsep (map printExpr args)
printExpr (Unary o t) = parens $ printOp1 o <+> printExpr t
printExpr (Lit l) = printLit l

printOp1 :: Op1 -> Doc ann
printOp1 Suc = "Nat.succ"  -- use `Nat.succ` explicitly

printOp2 :: Op2 -> Doc ann
printOp2 Plus = "+"

printLocalDefn :: LocalDefn -> Doc ann
printLocalDefn (LocDefFun var Nothing args expr) =
  prettyArgs var printArg args <+> assign <+> printExpr expr
printLocalDefn (LocDefFun var (Just t) args expr) =
  typeAnn (prettyArgs var printArg args) (printReturnType t) <+> assign <+> printExpr expr

printDef :: [Definition] -> Definition -> Doc ann
printDef _ (DefTVar var Nothing expr) = pretty var <+> assign <+> (printExpr expr)
printDef _ (DefTVar var (Just t) expr) = "def" <+> typeAnn (pretty var) (printType t) <+>
  assign <+> printExpr expr
printDef _ (DefFun var Nothing args expr) = pretty var <+> hsep (map (pretty . arg) args) <+> assign <+>
  (printExpr expr)
printDef _ (DefFun var (Just t) args expr) = "def" <+> typeAnn (pretty var <+> hsep (map printArg args))
  (printType t) <+> assign <+> (printExpr expr)
printDef _ (DefPatt var params ty _ cons) =
    "def" <+> typeAnn (pretty var) (printType (foldr Arr ty (map snd params))) <> hardline <>
    vsep (map (\(a, e) -> pipe <+> (hsep $ map (pretty . arg) a) <+> "=>" <+> (printExpr e)) cons)
printDef _ (DefDataType var args t) =
  "inductive" <+> typeAnn (pretty var) (printType t) <+> "where" <> hardline <>
   vsep (map (\(x, y) -> pipe <+> typeAnn (pretty x) (printType y)) args)
printDef _ (DefPDataType var params args t) =
   "inductive" <+>
       typeAnn (pretty var <+> hsep (map (\(x, y) -> teleCell (pretty x) (printType y)) params))
       (printType t) <+> "where" <> hardline <>
   vsep (map (\(x, y) -> pipe <+> typeAnn (pretty x) (printType y)) args)
   -- unwords (map (\(x, y) -> parens (x ++ typedel ++ printType y)) params) ++ typedel ++
   -- printType t ++ " where " ++ unwords (map (\(x, y) -> "\n| " ++ x ++ typedel ++ (printType y)) args)

-- records Def
printDef _ (DefRecType name params consName fields _) =
    "structure" <+> prettyParams <+> "where" <> hardline <>
    indent 4 (pretty consName <+> "::" <> hardline <>
    vsep (map (\(fname, ftype) -> typeAnn (pretty fname) (printType ftype)) fields)) <>
    hardline
      where
        prettyParams = case params of
            [] -> pretty name
            _ -> pretty name <+> hsep (map (\(Arg n t) -> teleCell (pretty n) (printType t)) params)


-- OpenLine: It takes a list of record definitions (recs) and uses it to build an open line.
-- Exclusive lean syntax needed for simplicity
printDef recs (DefRec name recType consName fields) =
    openLine <>
    typeAnn (pretty name) (printType recType) <+> assign <+> pretty consName <+>
    hsep (map (printExpr . snd) fields)
  where
    recNamesList = [ rName | DefRecType rName _ _ _ _ <- recs ]
    openLine = if null recNamesList then emptyDoc else "open" <+> hsep (map pretty recNamesList) <> hardline
printDef _ (OpenName n) = "open" <+> pretty n
printDef _ (Separator '\n' n _) = blanklines n
printDef _ (Separator c n b) =
  let s = hcat $ replicate (fromIntegral n) (pretty c) in
  if b then hardline <> s <> hardline else s


printModule :: Module -> Lean ann
printModule (Module _ imports defs) =
    let
        headers = foldr printWithImport emptyDoc imports
        ctx = [ d | d@(DefRecType _ _ _ _ _) <- defs ]  -- extract record definitions from the module
        body = vcat (map (printDef ctx) defs)
    in Lean $ if null imports then body else headers <> hardline <> body

render :: Module -> String
render = renderString . layoutPretty defaultLayoutOptions . get . printModule

runLean :: Module -> IO()
runLean m = writeFile ("out/" ++ modname m ++ ".lean") $ render m
