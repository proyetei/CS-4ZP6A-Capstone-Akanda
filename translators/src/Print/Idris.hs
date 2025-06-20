module Print.Idris
  ( printModule
  , render
  , runIdris
  ) where

import qualified Prettyprinter as P
import Prettyprinter.Render.String (renderString)

import Data.List (intercalate)

import Grammar
import qualified Print.Generic as G

newtype Idris ann = Idris {get :: P.Doc ann}

-- to be migrated
{-
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
-}

import_, univ, arr, typedel, assign :: String
import_ = "import "
univ = "Type"
arr = " -> "
typedel = " : "
assign = " = "

printImport :: Import -> String
printImport (ImportLib VecMod) = import_ ++ "Data.Vect"
-- There rest are builtin
printImport (ImportLib NatMod) = ""
printImport (ImportLib StringMod) = ""
printImport (ImportLib ListMod) = ""

printType :: Type -> String
printType (Univ) = univ
printType (Con t) = t
printType (Arr t1 t2) = printType t1 ++ arr ++ printType t2
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
  typeSig ++ var ++ targ ++ assign ++ printExpr expr
    where typeSig = maybe "" (\t -> var ++ typedel ++ printType t ++ "\n    ") ty
          targ = if null args then "" else " " ++ intercalate " " (map arg args)

printDef :: Definition -> String
printDef (DefTVar var Nothing expr) = var ++ assign ++ printExpr expr
printDef (DefTVar var (Just t) expr) = G.line (var ++ typedel ++ printType t) ++ var ++ assign ++ printExpr expr
printDef (DefFun var ty args expr) = printLocalDefn (LocDefFun var ty args expr)
printDef (DefPatt var params ty _ cons) =
    var ++ typedel ++ printType (foldr Arr ty (map snd params)) ++
    unwords (map (\(a,e) -> "\n" ++ var ++ " " ++ (unwords $ map arg a) ++ assign ++ printExpr e ) cons)
printDef (DefDataType name cons ty) = "data " ++ name ++ typedel ++ printType ty ++ " where" ++
    (G.line $ unwords (map (\(n, t) -> "\n " ++ n ++ typedel ++ printType t) cons))
printDef (DefPDataType name params cons ty) = "data " ++ name ++ typedel ++
    foldr (\(x, y) z -> G.parens (x ++ typedel ++ printType y) ++ arr ++ z) (printType ty) params ++ " where" ++ 
    unwords (map (\(n, t) -> "\n " ++ n ++ typedel ++ G.line (intercalate arr (map fst params) ++ arr ++ printType t)) cons)

-- Record Defn
printDef (DefRecType name params consName fields _) =
    "record " ++ name ++ paramsStr ++ " where\n    constructor " ++ consName ++ "\n" ++
    unlines (map (\(fname, ftype) -> "    " ++ fname ++ typedel ++ printType ftype) fields)
  where
    paramsStr = case params of
        [] -> ""
        _ -> " " ++ unwords (map (\(Arg n t) -> G.parens $ n ++ typedel ++ printType t) params)

printDef (DefRec name recType consName fields) =
    (G.line $ name ++ typedel ++ printType recType) ++
    name ++ assign ++ consName ++ " " ++ (G.line $ intercalate " " (map (printExpr . snd) fields))

printDef (OpenName _) = ""
printDef (Separator c n b) =
  let s = replicate (fromIntegral n) c in
  if b then '\n' : G.line s else s

printModule :: Module -> Idris ann
printModule (Module _ imports defs) =
    let
        headers = P.pretty "module Main" <> P.hardline <>
          (if null imports then mempty else P.vsep (map (P.pretty . printImport) imports) <> P.hardline)
        -- Concatenate all definitions
        body = P.vcat $ map (P.pretty . printDef) defs

    in Idris $ headers <> P.hardline <> body <> P.hardline <>
               P.pretty "main : IO()" <> P.hardline <>
               P.pretty "main = putStrLn " <> P.dquote <> P.dquote

render :: Module -> String
render = renderString . P.layoutPretty P.defaultLayoutOptions . get . printModule

runIdris :: Module -> IO()
runIdris m = writeFile ("out/" ++ modname m ++ ".idr") $ render m
