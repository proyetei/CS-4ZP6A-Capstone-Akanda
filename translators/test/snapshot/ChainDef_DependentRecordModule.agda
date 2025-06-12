module ChainDef_DependentRecordModule where 
open import Agda.Builtin.Nat

record Record1 : Set where
    constructor Const1
    field
        f1 : Nat
record Record2 : Set where
    constructor Const2
    field
        f2 : Record1
record Record3 : Set where
    constructor Const3
    field
        f3 : Record2
record Record4 : Set where
    constructor Const4
    field
        f4 : Record3
record Record5 : Set where
    constructor Const5
    field
        f5 : Record4

example : Record5
example = Const5 (Const4 (Const3 (Const2 (Const1 10))))