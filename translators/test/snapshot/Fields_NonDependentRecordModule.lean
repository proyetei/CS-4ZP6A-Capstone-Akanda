import Init.Data.Vector

structure X where
    Const ::
    f1 : Nat
    f2 : Nat
    f3 : Nat
    f4 : Nat
    f5 : Nat

open X
example : X := Const 1 1 1 1 1