module Grammar (Module (..), Definition (..), Type (..), Arg (..), Expr (..)) where

-- grammar

data Module = Module { mod :: Name, defs :: [Definition] } -- is there anything like 'module Main where' FEL?
        | File { fil :: Name, con :: String } -- this option is for empty files and other invalid programs

{- data Import = ImportLib Lib   -- i feel like it doesn't make sense to define a universal import because libraries will be language-specific
            | ImportFun Name Lib
-}

data Definition = DefFun Name (Maybe Type) [Arg] Expr
                | DefNesFun Name (Maybe Type) [Arg] Expr -- Constructor for nested functions
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
        | FunCall Name [Expr]           --constructor to call function

-- aliases for readability purposes
type Name = String
type Lib = String
type Op = String
