module Fields_DependentRecordModule where
open import Agda.Builtin.Nat
open import Data.Vec.Base

record Cap_X : Set where
    constructor Const
    field
        f1 : Nat
        f2 : Vec Nat f1
        f3 : Vec Nat (suc f1)
        f4 : Vec Nat (suc (suc f1))
        f5 : Vec Nat (suc (suc (suc f1)))

example : Cap_X
example = Const (1 ∷ [])
    (1 ∷ 1 ∷ [])
    (1 ∷ 1 ∷ 1 ∷ [])
    (1 ∷ 1 ∷ 1 ∷ 1 ∷ [])
    (1 ∷ 1 ∷ 1 ∷ 1 ∷ 1 ∷ [])