module Parameters_DependentRecordModule where
open import Agda.Builtin.Nat

record X (f1 : Nat) (f2 : Nat) (f3 : Nat) (f4 : Nat) (f5 : Nat) : Set where
    constructor Const
    field
        sums : Nat

example : X 1 2 3 4 5
example = Const (1 + 2 + 3 + 4 + 5)