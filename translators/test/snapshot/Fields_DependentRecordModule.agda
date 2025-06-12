module Fields_DependentRecordModule where 
open import Agda.Builtin.IO  
open import Agda.Builtin.Nat 
open import Data.Vec 
open import Agda.Builtin.List 

record X : Set where
    constructor Const
    field
        f1 : Nat
        f2 : Vec Nat f1
        f3 : Vec Nat (suc f1)
        f4 : Vec Nat (suc (suc f1))
        f5 : Vec Nat (suc (suc (suc f1)))

example : X
example = Const 1(1 ∷ [])(1 ∷ 1 ∷ [])(1 ∷ 1 ∷ 1 ∷ [])(1 ∷ 1 ∷ 1 ∷ 1 ∷ [])