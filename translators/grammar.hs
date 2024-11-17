module Grammar (Module (..), Definition (..), Type (..), Arg (..), Expr (..)) where

-- grammar

data Module = Module { mod :: Name, defs :: [Definition] } -- is there anything like 'module Main where' FEL?

{- data Import = ImportLib Lib   -- i feel like it doesn't make sense to define a universal import because libraries will be language-specific
            | ImportFun Name Lib
-}

data Definition = DefFun Name (Maybe Type) [Arg] Expr
                | DefVar Name (Maybe Type) Expr

data Type = Con Name
        | Arr Type Type

data Arg = Arg { arg :: Name, ty :: Type }

data Expr = Var Name
        | Int Int 
        | Bool Bool 
        | String String 
        | Mon Op Expr
        | Bin Op Expr Expr
        | Let [Definition] Expr
        | If Expr Expr Expr 
        | Where Expr [Definition]

-- aliases for readability purposes
type Name = String
type Lib = String
type Op = String
