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

printType :: Type -> String
printType (Univ) = "Type" -- fix
printType (Con t) = t
printType (Arr t1 t2) = printType t1 ++ " -> " ++ printType t2 -- fix
printType (TVar t) = t
printType (PCon "Vec" [Con baseType, size]) = "Vect " ++ printType size ++ " " ++ baseType
printType (PCon name types) = name ++ " " ++ unwords (map printType types)
printType (DCon name [] exprs) = -- For dependent type constructors
    name ++ " " ++ unwords (map printExpr exprs)
printType (DCon name types exprs) = -- For dependent type constructors
    name ++ " " ++ unwords (map printType types) ++ " " ++ unwords (map printExpr exprs)
printType (Index names ty) = G.brackets $ intercalate ", " names ++ " : " ++ printType ty
printType (Embed e) = printExpr e

printExpr :: Expr -> String
printExpr (Constructor name) = name
printExpr (Var var) = var
printExpr (Nat n) = show n
printExpr (String str) = G.quote str
printExpr (Paren e) = G.parens $ printExpr e
printExpr (Bin op e1 e2) = printExpr e1 ++ printOp op ++ printExpr e2
printExpr (Let [] expr) = printExpr expr -- this should never happen
printExpr (Let (d:[]) expr) = "let \n    " ++ printLocalDefn d ++ " in \n    " ++ printExpr expr
printExpr (Let (d:ds) expr) = 
  "let \n    " ++ intercalate "\n    " (map printLocalDefn (d:ds)) ++
  "\n    in \n    " ++ printExpr expr -- probably need to recursively indent blocks to make sure everything stays aligned
printExpr (If cond thn els) = "if " ++ printExpr cond ++ " then " ++ printExpr thn ++ " else " ++ printExpr els
printExpr (Where expr ds) = printExpr expr ++ "\n    where " ++ intercalate "\n    " (map printLocalDefn ds)
printExpr (FunCall fun args) = fun ++ " " ++ (unwords $ map printExpr args) -- Added case for FunCall
printExpr (VecE l) = G.sqbrackets $ intercalate ", " (map printExpr l)
printExpr (ListE l) = G.sqbrackets $ intercalate ", " (map printExpr l)
printExpr (Suc t) = G.parens $ "S " ++ printExpr t

printOp :: Op -> String
printOp Plus = " + "

printLocalDefn :: LocalDefn -> String
printLocalDefn (LocDefFun var ty args expr) = 
  typeSig ++ var ++ targ ++ " = " ++ printExpr expr  -- fix: assign
    where typeSig = maybe "" (\t -> var ++ typedel ++ printType t ++ "\n    ") ty
          targ = if null args then "" else " " ++ intercalate " " (map arg args)

printDef :: Definition -> Doc ann
printDef (DefTVar var Nothing expr) = 
  pretty var <+> assign <+> P.align (pretty $ printExpr expr) <> softline'
printDef (DefTVar var (Just t) expr) = 
  typeAnn (pretty var) (pretty $ printType t) <> hardline <>
  pretty var <+> assign <+> P.align (pretty $ printExpr expr) <> hardline

printDef (DefFun var ty args expr) = pretty $ printLocalDefn (LocDefFun var ty args expr)
printDef (DefPatt var params ty _ cons) =
    typeAnn (pretty var) (pretty $ printType (foldr Arr ty (map snd params))) <> P.line <>
    P.vsep (map (\(a, e) -> (pretty var) <+> P.hsep (map (pretty . arg) a) <+> assign <+> pretty (printExpr e)) cons)
printDef (DefDataType name cons ty) =
  data_ <+> typeAnn (pretty name) (pretty $ printType ty) <+> "where" <> hardline <>
  indent 1 (vsep (map (\(n, t) -> typeAnn (pretty n) (pretty $ printType t)) cons)) <>
   P.hardline
printDef (DefPDataType name params cons ty) =
  data_ <+> 
    typeAnn (pretty name) (P.concatWith (\x y -> x <+> arr <+> y) (map (\(x, y) -> teleCell (pretty x) (pretty $ printType y)) params)) <+> arr <+> (pretty $ printType ty) <+> 
    "where" <> hardline <>
    indent 1 (vsep (map (\(n,t) -> typeAnn (pretty n) 
                                   (P.encloseSep P.emptyDoc (P.space <> arr) (P.space <> arr <> P.space) (map (pretty.fst) params)) <+> pretty (printType t)) cons)) <>
    hardline

printDef (DefRecType name params consName fields _) =
    rec <+> pp_params <+> "where" <> hardline <>
    indent 4 (vsep $ "constructor" <+> pretty consName :
       (vsep $ map (\(fname, ftype) -> typeAnn (pretty fname) (pretty $ printType ftype)) fields)
       : []) <>
    hardline
    where
      ll = map (\(Arg n t) -> teleCell (pretty n) (pretty $ printType t)) params
      pp_params = if null params then pretty name else pretty name <+> hsep ll

printDef (DefRec name recType consName fields) =
    typeAnn (pretty name) (pretty $ printType recType) <> hardline <> 
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
