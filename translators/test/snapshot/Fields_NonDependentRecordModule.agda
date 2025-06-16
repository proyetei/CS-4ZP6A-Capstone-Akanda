module Fields_NonDependentRecordModule where
open import Agda.Builtin.Nat

record Cap_X : Set where
    constructor Const
    field
        f1 : Nat
        f2 : Nat
        f3 : Nat
        f4 : Nat
        f5 : Nat

example : Cap_X
example = Const 1 1 1 1 1