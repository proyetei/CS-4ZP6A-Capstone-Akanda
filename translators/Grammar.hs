module Grammar (Module (..), Import (..), Definition (..), Type (..), Arg (..), Expr (..)) where

-- grammar

data Module = Module { mod :: Name, imports :: [Import], defs :: [Definition] } -- is there anything like 'module Main where' FEL?
        | File { fil :: Name, con :: String } -- this option is for empty files and other invalid programs

data Import = ImportLib Lib   
            | ImportFun Name Lib

data Definition = DefFun Name (Maybe Type) [Arg] Expr
                | DefNesFun Name (Maybe Type) [Arg] Expr -- Constructor for nested functions
                | DefPatt Name [(Name,Type)] Type Name [([Arg], Expr)] --function name; name,type is parameters for roq; output type; name is input to match with for coq, constructors
                | DefVar Name (Maybe Type) Expr
                | DefDataType Name [(Name,Type)] Type -- datatype name, constructors, usually type is Set
                | DefPDataType Name [(Name, Type)] [(Name,Type)] Type --datatype name, parameters, constrcutors, overall type
                | DefRecType Name [Arg] Name [(Name,Type)] Type -- [Arg] for parameters (empty list if no params), (Maybe Name) is the type constructor
                | DefRec Name Type Name [(String, Expr)] -- record name, record type, possible constructor type (this auto fills in, only needed for Chain dependent constructor test)
                | OpenName Name --just for Lean, to refer to user-defined datatypes directly
                | DefModule Module -- for nested modules

data Type = Con Name -- type constructor
        | PCon Name [Type] -- parameterized type constructor
        | DCon Name [Type] [Expr] -- dependent type constructor (note that a dependent type is also parameterized)
        | Arr Type Type -- function type
        | TVar Name -- type variable
        | Suc Type
        | Index [Name] Type

data Arg = Arg { arg :: Name, ty :: Type }

data InitRecord = InitRecord { 
    recordName :: String, 
    fieldAssignments :: [(String, Expr)] 
} 

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
        | VecEmpty             -- Represents `[]`
        | VecCons Expr Expr    -- Represents `_∷_`, e.g., `1 ∷ []`
        | ListEmpty             -- Represents `[]`
        | ListCons Expr Expr      -- Represents list construction, e.g. `1 :: []`
        | Paren Expr
        | Constructor Name
        
        
-- aliases for readability purposes
type Name = String
type Lib = String
type Op = String
