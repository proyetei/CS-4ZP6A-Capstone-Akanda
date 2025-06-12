import Init.Data.Vector

structure X where
    Const ::
    f1 : Nat
    f2 : Vector Nat f1
    f3 : Vector Nat (Nat.succ f1)
    f4 : Vector Nat (Nat.succ (Nat.succ f1))
    f5 : Vector Nat (Nat.succ (Nat.succ (Nat.succ f1)))

open X
example : X := Const 1 ⟨#[1], rfl⟩ ⟨#[1, 1], rfl⟩ ⟨#[1, 1, 1], rfl⟩ ⟨#[1, 1, 1, 1], rfl⟩