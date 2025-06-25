
structure X (f1 : Nat) (f2 : Nat) (f3 : Nat) (f4 : Nat) (f5 : Nat) where
    Const ::
    sums : Nat

open X
example : X 1 2 3 4 5 := Const (1 + 2 + 3 + 4 + 5)