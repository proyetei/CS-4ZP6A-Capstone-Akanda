{-# Language OverloadedStrings #-}
module Print.Lean
  ( printModule
  , runLean
  , render
  ) where

import Data.List (intercalate)

import Prettyprinter
import Prettyprinter.Render.String (renderString)

import Grammar
import qualified Print.Generic as G

newtype Lean ann = Lean {get :: Doc ann}

class Keywords rep where
  import_ :: rep
  assign  :: rep
{-
  data_   :: rep
  rec     :: rep
  univ    :: rep
  arr     :: rep
-}
instance Keywords (Doc ann) where
  import_ = "import"
  assign  = ":="
{-
  data_   = "data"
  rec     = "record"
  univ    = "Type"
  arr     = "->"
-}

class TypeAnn rep where
  typeAnn :: rep -> rep -> rep
  teleCell :: rep -> rep -> rep

instance TypeAnn (Doc ann) where
  typeAnn trm typ = trm <+> ":" <+> typ
  teleCell trm typ = parens $ trm <+> ":" <+> typ

univ, arr, typedel :: String
univ = "Type"
arr = " -> "
typedel = " : "

-- append an Import if needed
printWithImport :: Import -> Doc ann -> Doc ann
printWithImport (ImportLib VecMod) m = m <> import_ <+> "Init.Data.Vector" <> hardline
-- There rest are builtin
printWithImport (ImportLib NatMod) m = m
printWithImport (ImportLib StringMod) m = m
printWithImport (ImportLib ListMod) m = m

-- Print types (unchanged)
printType :: Type -> String
printType (Univ) = univ
printType (Con t) = t
printType (Arr t1 t2) = printType t1 ++ arr ++ printType t2
printType (TVar t) = t
printType (PCon "Vec" args) = "Vector " ++ unwords (map printType args)
printType (PCon name types) = name ++ " " ++ unwords (map printType types)
printType (DCon name [] exprs) = name ++ " " ++ unwords (map printExpr exprs)
printType (DCon name types exprs) = name ++ " " ++ unwords (map printType types) ++ " " ++ unwords (map printExpr exprs)
printType (Index names ty) = G.brackets $ unwords names ++ typedel ++ printType ty
printType (Embed e) = printExpr e

printReturnType :: Type -> String
printReturnType (Con t) = t
printReturnType (Arr _ t) = printReturnType t
printReturnType _ = error "show not occur as a return type"

printArg :: Arg -> String
printArg a = G.parens $ arg a ++ typedel ++ printType (argty a)

printArg' :: Arg -> Doc ann
printArg' a = teleCell (pretty $ arg a) (pretty $ printType $ argty a)

-- Print expressions (unchanged)
printExpr :: Expr -> String
printExpr (Constructor name) = name
printExpr (Var var) = var
printExpr (Nat n) = show n
printExpr (String str) = G.quote str
printExpr (Paren e) = G.parens $ printExpr e
printExpr (Bin op e1 e2) = printExpr e1 ++ printOp op ++ printExpr e2
printExpr (Let [] expr) = printExpr expr
printExpr (Let (d:[]) expr) = G.line ("let " ++ (printLocalDefn d)) ++ printExpr expr
printExpr (Let (d:ds) expr) = "let " ++ intercalate "\nlet " (map printLocalDefn (d:ds)) ++ "\n" ++ printExpr expr
printExpr (If cond thn els) = "if " ++ printExpr cond ++ " then \n\t" ++ printExpr thn ++ "\nelse " ++ printExpr els
printExpr (Where expr ds) = printExpr expr ++ "\n\twhere " ++ intercalate "\n\t" (map printLocalDefn ds)
printExpr (FunCall fun args) = fun ++ " " ++ unwords (map printExpr args)
printExpr (VecE l) = '#' : G.sqbrackets (intercalate ", " (map printExpr l))
printExpr (ListE l) = G.sqbrackets $ intercalate ", " (map printExpr l)
printExpr (Suc t) = G.parens $ "Nat.succ " ++ printExpr t  -- Use `Nat.succ` explicitly

printOp :: Op -> String
printOp Plus = " + "

printLocalDefn :: LocalDefn -> String
printLocalDefn (LocDefFun var Nothing args expr) = var ++ targs ++ " := " ++ printExpr expr
  where targs = if null args then "" else " " ++ intercalate " " (map printArg args)
  -- same as DefFun
printLocalDefn (LocDefFun var (Just t) args expr) = var ++ targs ++ typedel ++ printReturnType t ++ " := " ++ printExpr expr
  where targs = if null args then "" else " " ++ intercalate " " (map printArg args)

printDef :: [Definition] -> Definition -> Doc ann
printDef _ (DefTVar var Nothing expr) = pretty var <+> assign <+> (pretty $ printExpr expr)
printDef _ (DefTVar var (Just t) expr) = "def" <+> typeAnn (pretty var) (pretty $ printType t) <+>
  assign <+> (pretty $ printExpr expr) 
printDef _ (DefFun var Nothing args expr) = pretty var <+> hsep (map (pretty . arg) args) <+> assign <+>
  (pretty $ printExpr expr)
printDef _ (DefFun var (Just t) args expr) = "def " <+> typeAnn (pretty var <+> hsep (map printArg' args)) 
  (pretty $ printType t) <+> assign <+> (pretty $ printExpr expr)
printDef _ (DefPatt var params ty _ cons) =
    "def" <+> typeAnn (pretty var) (pretty $ printType (foldr Arr ty (map snd params))) <> hardline <>
    vsep (map (\(a, e) -> pipe <+> (hsep $ map (pretty . arg) a) <+> "=>" <+> (pretty $ printExpr e)) cons) 
printDef _ (DefDataType var args t) =
  "inductive" <+> typeAnn (pretty var) (pretty $ printType t) <+> "where" <> hardline <>
   vsep (map (\(x, y) -> pipe <+> typeAnn (pretty x) (pretty $ printType y)) args)
printDef _ (DefPDataType var params args t) =
   "inductive" <+> 
       typeAnn (pretty var <+> hsep (map (\(x, y) -> teleCell (pretty x) (pretty $ printType y)) params))
       (pretty $ printType t) <+> "where" <> hardline <>
   vsep (map (\(x, y) -> pipe <+> typeAnn (pretty x) (pretty $ printType y)) args)
   -- unwords (map (\(x, y) -> G.parens (x ++ typedel ++ printType y)) params) ++ typedel ++
   -- printType t ++ " where " ++ unwords (map (\(x, y) -> "\n| " ++ x ++ typedel ++ (printType y)) args)

-- records Def
printDef _ (DefRecType name params consName fields _) =
    "structure" <+> prettyParams <+> "where" <> hardline <>
    indent 4 (pretty consName <+> "::" <> hardline <>
    vsep (map (\(fname, ftype) -> typeAnn (pretty fname) (pretty $ printType ftype)) fields)) <>
    hardline
      where
        prettyParams = case params of
            [] -> pretty name
            _ -> pretty name <+> hsep (map (\(Arg n t) -> teleCell (pretty n) (pretty $ printType t)) params)


-- OpenLine: It takes a list of record definitions (recs) and uses it to build an open line.
-- Exclusive lean syntax needed for simplicity
printDef recs (DefRec name recType consName fields) =
    openLine <> 
    typeAnn (pretty name) (pretty $ printType recType) <+> assign <+> pretty consName <+>
    hsep (map (pretty . printExpr . snd) fields)
  where
    recNamesList = [ rName | DefRecType rName _ _ _ _ <- recs ]
    openLine = if null recNamesList then emptyDoc else "open" <+> hsep (map pretty recNamesList) <> hardline
printDef _ (OpenName n) = "open" <+> pretty n
printDef _ (Separator '\n' n _) = G.blanklines n
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
