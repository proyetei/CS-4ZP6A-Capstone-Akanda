module Parameters_DependentRecordModule where 
open import Agda.Builtin.IO  
open import Agda.Builtin.Nat 
open import Data.Vec 
open import Agda.Builtin.List 

record X (f1 : Nat) (f2 : Nat) (f3 : Nat) (f4 : Nat) (f5 : Nat) : Set where
    constructor Const
    field
        sums : Nat
        values : List Nat

example : X 1 2 3 4 5
example = Const(1 + 2 + 3 + 4 + 5) (1 ∷ 2 ∷ 3 ∷ 4 ∷ 5 ∷ [])