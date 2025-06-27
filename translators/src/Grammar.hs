{-# Language OverloadedStrings #-}
module Grammar (Module (..), Import (..), Definition (..), Tm (..), Arg (..)
  , KnownMods (..), Op1 (..), Op2 (..), LocalDefn (..), Literal (..)
  , Name
  , modname
  , nat, con, num, bool, list, vec, string, suc, plus, app1, appnm) where

import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
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
  = DefPatt Name [(Name,Tm)] Tm Name [([Arg Name Tm], Tm)]
    -- ^ Function name; name,type is parameters for Rocq; output type; name is input to match with for Rocq, constructors
  | DefTVar Name (Maybe Tm) Tm
    -- ^ Define a (top-level) variable with an optional type annotation
  | DefDataType Name [(Name,Tm)] Tm
    -- ^ Datatype name, constructors, usually type is Set
  | DefPDataType Name [(Name, Tm)] [(Name,Tm)] Tm
    -- ^ Datatype name, parameters, constructors, overall type
  | DefRecType Name [Arg Name Tm] Name [(Name,Tm)] Tm
    -- ^ [Arg] for parameters (empty list if no params), (Maybe Name) is the type constructor
  | DefRec Name Tm Name [(Name, Tm)]
    -- ^ Record name, record type, possible constructor type (this auto fills in, only needed for Chain dependent constructor test)
  | OpenName Name
    -- ^ Just for Lean, to refer to user-defined datatypes directly
  | Separator Char Natural Bool
    -- ^ To allow a "separator line" in the produced code, of that character repeated n times. 
    -- It is on a line of its own if True, spit out as-is and in-place if false

data LocalDefn
  = LocDefFun Name (Maybe Tm) [Arg Name Tm] Tm

data Tm
  = PCon Name [Tm]        -- (parameterized) type constructor
  | DCon Name [Tm]        -- dependent type constructor (note that a dependent type is also parameterized)
  | Arr Tm Tm             -- function type
  | Index [Name] Tm
  | Univ                  -- a Universe, aka "Type" itself, called "Set" in Agda
  | Var Name
  | Binary Op2 Tm Tm      -- only for known, hard-coded binary operations
  | Unary Op1 Tm          -- only for known, hard-coded unary operations
  | Let [LocalDefn] Tm
  | If Tm Tm Tm
  | Where Tm [LocalDefn]
  | App Tm (NonEmpty Tm)
  | Paren Tm
  | Constructor Name
  | Lit Literal
  -- | Lam                  -- we don't as-yet use it?

data Arg a b = Arg { arg :: a, argty :: b }

data Literal
  = Nat Natural
  -- ^ Natural number literals.
  -- We will attempt to translate these as literals like @100@
  -- instead of @succ@ and @zero@ constructors.
  | Bool Bool
  -- ^ Boolean literals.
  | List [Tm]
  -- ^ List literals.
  -- We will attempt to translate these as literals like @[x, y, z]@
  -- as opposed to cons constructors.
  | Vec [Tm]
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

nat :: Tm
nat = PCon "Nat" []

con :: Name -> Tm
con n = PCon n []

num :: Natural -> Tm
num = Lit . Nat

bool :: Bool -> Tm
bool = Lit . Bool

list :: [ Tm ] -> Tm
list = Lit . List

vec :: [ Tm ] -> Tm
vec = Lit . Vec

string :: String -> Tm
string = Lit . String

suc :: Tm -> Tm
suc = Unary Suc

plus :: Tm -> Tm -> Tm
plus = Binary Plus

app1 :: Name -> Tm -> Tm
app1 a b = App (Var a) (NE.singleton b)

appnm :: Name -> NonEmpty Tm -> Tm
appnm a b = App (Var a) b
