module Fields_NonDependentRecordModule where 
open import Agda.Builtin.IO  
open import Agda.Builtin.Nat 
open import Data.Vec 
open import Agda.Builtin.List 

record X : Set where
    constructor Const
    field
        f1 : Nat
        f2 : Nat
        f3 : Nat
        f4 : Nat
        f5 : Nat

example : X
example = Const 1 1 1 1 1