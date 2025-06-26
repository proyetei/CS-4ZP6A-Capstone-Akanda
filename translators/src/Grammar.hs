{-# Language OverloadedStrings #-}
module Grammar (Module (..), Import (..), Definition (..), Type (..), Arg (..), Expr (..)
  , KnownMods (..), Op1 (..), Op2 (..), LocalDefn (..), Literal (..)
  , Name
  , modname
  , nat, con, num, bool, list, vec, string, suc, plus) where

import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty)
import Numeric.Natural (Natural)

-- grammar

data Module = Module 
  { mname :: Name
  , mimports :: [Import]
  , mdefs :: [Definition]
  }

modname :: Module -> Name
modname m = mname m

data KnownMods = NatMod | ListMod | VecMod | StringMod

newtype Import = ImportLib KnownMods

data Definition
  = DefFun Name (Maybe Type) [Arg] Expr
  | DefPatt Name [(Name,Type)] Type Name [([Arg], Expr)]
    -- ^ Function name; name,type is parameters for roq; output type; name is input to match with for coq, constructors
  | DefTVar Name (Maybe Type) Expr
    -- ^ Define a variable (i.e. 'let') with an optional type annotation
  | DefDataType Name [(Name,Type)] Type
    -- ^ Datatype name, constructors, usually type is Set
  | DefPDataType Name [(Name, Type)] [(Name,Type)] Type
    -- ^ Datatype name, parameters, constructors, overall type
  | DefRecType Name [Arg] Name [(Name,Type)] Type
    -- ^ [Arg] for parameters (empty list if no params), (Maybe Name) is the type constructor
  | DefRec Name Type Name [(Name, Expr)]
    -- ^ Record name, record type, possible constructor type (this auto fills in, only needed for Chain dependent constructor test)
  | OpenName Name
    -- ^ Just for Lean, to refer to user-defined datatypes directly
  | Separator Char Natural Bool
    -- ^ To allow a "separator line" in the produced code, of that character repeated n times. 
    -- It is on a line of its own if True, spit out as-is and in-place if false

data LocalDefn
  = LocDefFun Name (Maybe Type) [Arg] Expr

data Type 
        = PCon Name [Type]        -- (parameterized) type constructor
        | DCon Name [Type] [Expr] -- dependent type constructor (note that a dependent type is also parameterized)
        | Arr Type Type           -- function type
        | TVar Name               -- type variable
        | Embed Expr              -- Exprs seen as a type (should later merge properly)
        | Index [Name] Type
        | Univ                    -- a Universe, aka "Type" itself, called "Set" in Agda

data Arg = Arg { arg :: Name, argty :: Type }

data Expr 
  = Var Name
  | Binary Op2 Expr Expr    -- only for known, hard-coded binary operations
  | Unary Op1 Expr          -- only for known, hard-coded unary operations
  | Let [LocalDefn] Expr
  | If Expr Expr Expr
  | Where Expr [LocalDefn]
  | App Name (NonEmpty Expr)
  | Paren Expr
  | Constructor Name
  | Lit Literal
  -- | Lam                  -- we don't as-yet use it?

data Literal
  = Nat Natural
  -- ^ Natural number literals.
  -- We will attempt to translate these as literals like @100@
  -- instead of @succ@ and @zero@ constructors.
  | Bool Bool
  -- ^ Boolean literals.
  | List [Expr]
  -- ^ List literals.
  -- We will attempt to translate these as literals like @[x, y, z]@
  -- as opposed to cons constructors.
  | Vec [Expr]
  -- ^ Vector literals.
  -- We will attempt to translate these as literals like @[x, y, z]@
  -- as opposed to @cons@ and @nil@ constructors.
  | String String
  -- ^ String literals.

data Op2 = Plus
data Op1 = Suc

-- aliases for readability purposes
type Name = Text


--------------------------
-- useful short-hands for things that are used often

nat :: Type
nat = PCon "Nat" []

con :: Name -> Type
con n = PCon n []

num :: Natural -> Expr
num = Lit . Nat

bool :: Bool -> Expr
bool = Lit . Bool

list :: [ Expr ] -> Expr
list = Lit . List

vec :: [ Expr ] -> Expr
vec = Lit . Vec

string :: String -> Expr
string = Lit . String

suc :: Expr -> Expr
suc = Unary Suc

plus :: Expr -> Expr -> Expr
plus = Binary Plus
