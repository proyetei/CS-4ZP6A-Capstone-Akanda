{-# Language OverloadedStrings #-}
module Print.Idris
  ( printModule
  , render
  , runIdris
  ) where

import Prettyprinter (Doc, (<+>), pretty, softline', hardline, indent, vsep, hsep, nest, sep)
import qualified Prettyprinter as P
import Prettyprinter.Render.String (renderString)

import Data.List (intercalate)

import Grammar
import qualified Print.Generic as G

newtype Idris ann = Idris {get :: Doc ann}

-- to be migrated
class Keywords rep where
  import_ :: rep
  assign  :: rep
  data_   :: rep
  rec     :: rep
  univ    :: rep
  arr     :: rep
{-
  lcons   :: rep
-}

instance Keywords (Doc ann) where
  import_ = "import"
  assign  = "="
  data_   = "data"
  rec     = "record"
  univ    = "Type"
  arr     = "->"
{-
  lcons   = "\x2237" 
-}
class TypeAnn rep where
  typeAnn :: rep -> rep -> rep
  teleCell :: rep -> rep -> rep

instance TypeAnn (Doc ann) where
  typeAnn trm typ = trm <+> ":" <+> typ
  teleCell trm typ = P.parens $ trm <+> ":" <+> typ

typedel :: String
typedel = " : "

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
printType (DCon name [] exprs) = -- For dependent type constructors
    pretty name <+> hsep (map (pretty . printExpr) exprs)
printType (DCon name types exprs) = -- For dependent type constructors
    pretty name <+> hsep (map printType types) <+> hsep (map (pretty . printExpr) exprs)
printType (Index names ty) = P.braces $ typeAnn (hsep $ P.punctuate P.comma $ map pretty names) (printType ty)
-- printType (Index names ty) = G.brackets $ intercalate ", " names ++ " : " ++ printType ty
printType (Embed e) = pretty $ printExpr e


printExpr :: Expr -> String
printExpr (Constructor name) = name
printExpr (Var var) = var
printExpr (Nat n) = show n
printExpr (String str) = G.quote str
printExpr (Paren e) = G.parens $ printExpr e
printExpr (Bin op e1 e2) = printExpr e1 ++ printOp op ++ printExpr e2
printExpr (Let [] expr) = printExpr expr -- this should never happen
printExpr (Let (d:[]) expr) = "let \n    " ++ (show $ printLocalDefn d) ++ " in \n    " ++ printExpr expr
printExpr (Let (d:ds) expr) = 
  "let \n    " ++ intercalate "\n    " (map (show . printLocalDefn) (d:ds)) ++
  "\n    in \n    " ++ printExpr expr -- probably need to recursively indent blocks to make sure everything stays aligned
printExpr (If cond thn els) = "if " ++ printExpr cond ++ " then " ++ printExpr thn ++ " else " ++ printExpr els
printExpr (Where expr ds) = printExpr expr ++ "\n    where " ++ intercalate "\n    " (map (show . printLocalDefn) ds)
printExpr (FunCall fun args) = fun ++ " " ++ (unwords $ map printExpr args) -- Added case for FunCall
printExpr (VecE l) = G.sqbrackets $ intercalate ", " (map printExpr l)
printExpr (ListE l) = G.sqbrackets $ intercalate ", " (map printExpr l)
printExpr (Suc t) = G.parens $ "S " ++ printExpr t

printOp :: Op -> String
printOp Plus = " + "

printLocalDefn :: LocalDefn -> Doc ann
printLocalDefn (LocDefFun var ty args expr) = 
  typeSig <> tvar <+> assign <+> P.align (pretty $ printExpr expr)
    where
        typeSig = case ty of
            Just t -> typeAnn (pretty var) (printType t) <> P.line
            Nothing -> mempty
        tvar = case args of
            [] -> pretty var
            (_:_) -> pretty var <+> (hsep $ map (pretty . arg) args)

printDef :: Definition -> Doc ann
printDef (DefTVar var Nothing expr) = 
  pretty var <+> assign <+> P.align (pretty $ printExpr expr) <> softline'
printDef (DefTVar var (Just t) expr) = 
  typeAnn (pretty var) (printType t) <> hardline <>
  pretty var <+> assign <+> P.align (pretty $ printExpr expr) <> hardline

printDef (DefFun var ty args expr) = printLocalDefn (LocDefFun var ty args expr)
printDef (DefPatt var params ty _ cons) =
    typeAnn (pretty var) (printType (foldr Arr ty (map snd params))) <> P.line <>
    P.vsep (map (\(a, e) -> (pretty var) <+> P.hsep (map (pretty . arg) a) <+> assign <+> pretty (printExpr e)) cons)
printDef (DefDataType name cons ty) =
  data_ <+> typeAnn (pretty name) (printType ty) <+> "where" <> hardline <>
  indent 1 (vsep (map (\(n, t) -> typeAnn (pretty n) (printType t)) cons)) <>
   P.hardline
printDef (DefPDataType name params cons ty) =
  data_ <+> 
    typeAnn (pretty name) (P.concatWith (\x y -> x <+> arr <+> y) (map (\(x, y) -> teleCell (pretty x) (printType y)) params)) <+> arr <+> (printType ty) <+> 
    "where" <> hardline <>
    indent 1 (vsep (map (\(n,t) -> typeAnn (pretty n) 
                                   (P.encloseSep P.emptyDoc (P.space <> arr) (P.space <> arr <> P.space) (map (pretty.fst) params)) <+> printType t) cons)) <>
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
    pretty name <+> assign <+> pretty consName <+> nest 4 (sep (map (pretty . printExpr . snd) fields)) <>
    hardline

printDef (OpenName _) = P.emptyDoc
printDef (Separator '\n' n _) = P.vcat $ replicate (fromIntegral n) P.emptyDoc
printDef (Separator c n b) = 
  let s = P.hcat $ replicate (fromIntegral n) (pretty c) in
  if b then hardline <> s <> hardline else s

printModule :: Module -> Idris ann
printModule (Module _ imports defs) =
    let
        headers = "module Main" <> P.hardline <>
          (if null imports then mempty else P.vsep (map printImport imports) <> P.hardline)
        -- Concatenate all definitions
        body = P.vcat $ map printDef defs

    in Idris $ headers <> P.hardline <> body <> P.hardline <>
               "main : IO()" <> P.hardline <>
               "main = putStrLn " <> P.dquote <> P.dquote

render :: Module -> String
render = renderString . P.layoutPretty P.defaultLayoutOptions . get . printModule

runIdris :: Module -> IO()
runIdris m = writeFile ("out/" ++ modname m ++ ".idr") $ render m
