import Init.Data.Vector

structure Cap_X where
    Const ::
    f1 : Nat
    f2 : Vector Nat f1
    f3 : Vector Nat (Nat.succ f1)
    f4 : Vector Nat (Nat.succ (Nat.succ f1))
    f5 : Vector Nat (Nat.succ (Nat.succ (Nat.succ f1)))

open Cap_X
example : Cap_X := Const #[1] #[1, 1] #[1, 1, 1] #[1, 1, 1, 1] #[1, 1, 1, 1, 1]