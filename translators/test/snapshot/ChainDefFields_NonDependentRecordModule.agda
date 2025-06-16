module ChainDefFields_NonDependentRecordModule where
open import Agda.Builtin.Nat

record Record1 : Set where
    constructor Const1
    field
        f1 : Nat
record Record2 : Set where
    constructor Const2
    field
        f2 : Nat
record Record3 : Set where
    constructor Const3
    field
        f3 : Nat
record Record4 : Set where
    constructor Const4
    field
        f4 : Nat
record Record5 : Set where
    constructor Const5
    field
        f5 : Nat

example : Record5
example = Const5 1