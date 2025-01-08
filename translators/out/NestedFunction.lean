
def n : Nat := let def f2(x1 : Nat) (x2 : Nat)  : Nat -> Nat -> Nat := x2+x1+1
let def f3(x1 : Nat) (x2 : Nat) (x3 : Nat)  : Nat -> Nat -> Nat -> Nat := x3+x2+x1+1
let def f1(x1 : Nat)  : Nat -> Nat := x1+1
f1 2+f2 2 3+f3 2 3 4
