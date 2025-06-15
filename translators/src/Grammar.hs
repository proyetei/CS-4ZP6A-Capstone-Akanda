module Grammar (Module (..), Import (..), Definition (..), Type (..), Arg (..), Expr (..)
  , KnownMods (..), Op (..)
  , modname
  , nat) where

import Numeric.Natural (Natural)

-- grammar

data Module
  = Module { mname :: Name
           , mimports :: [Import]
           , mdefs :: [Definition] } -- is there anything like 'module Main where' FEL?
  | File { mname :: Name
         , con :: String } -- this option is for modules with text instead of definitions -- should not generate invalid programs

modname :: Module -> Name
modname m = mname m

data KnownMods = NatMod | ListMod | VecMod | StringMod

newtype Import = ImportLib KnownMods

data Definition
  = DefFun Name (Maybe Type) [Arg] Expr
  | DefNesFun Name (Maybe Type) [Arg] Expr
     -- ^ Constructor for nested functions
  | DefPatt Name [(Name,Type)] Type Name [([Arg], Expr)]
    -- ^ function name; name,type is parameters for roq; output type; name is input to match with for coq, constructors
  | DefTVar Name Type Expr
    -- ^ define a variable (i.e. 'let') with a type annotation
  | DefUVar Name  Expr
    -- ^ define a variable (i.e. 'let')
  | DefDataType Name [(Name,Type)] Type
    -- ^ datatype name, constructors, usually type is Set
  | DefPDataType Name [(Name, Type)] [(Name,Type)] Type
    -- ^ datatype name, parameters, constrcutors, overall type
  | DefRecType Name [Arg] Name [(Name,Type)] Type
    -- ^ [Arg] for parameters (empty list if no params), (Maybe Name) is the type constructor
  | DefRec Name Type Name [(String, Expr)]
    -- ^ record name, record type, possible constructor type (this auto fills in, only needed for Chain dependent constructor test)
  | OpenName Name
    -- ^ just for Lean, to refer to user-defined datatypes directly
  | DefModule Module
    -- ^ for nested modules

data Type = Con Name              -- type constructor
        | PCon Name [Type]        -- parameterized type constructor
        | DCon Name [Type] [Expr] -- dependent type constructor (note that a dependent type is also parameterized)
        | Arr Type Type           -- function type
        | TVar Name               -- type variable
        | Embed Expr              -- Exprs seen as a type (should later merge properly)
        | Index [Name] Type
        | Univ                    -- a Universe, aka "Type" itself, called "Set" in Agda

data Arg = Arg { arg :: Name, argty :: Type }

data Expr = Var Name
        | Nat Natural
        | String String
        | Bin Op Expr Expr       -- only for known, hard-coded binary operations
        | Let [Definition] Expr
        | If Expr Expr Expr
        | Where Expr [Definition]
        | FunCall Name [Expr]    --constructor to call function
        | VecE [Expr]
        | ListE [Expr]
        | Paren Expr
        | Constructor Name
        | Suc Expr               -- hard-coded ??! FIXME


data Op = Plus

-- aliases for readability purposes
type Name = String


--------------------------
-- useful short-hands for things that are used often

nat :: Type
nat = Con "Nat"
