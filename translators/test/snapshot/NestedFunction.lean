
def n : Nat := let f1 (x1 : Nat) : Nat := 1 + x1
let f2 (x1 : Nat) (x2 : Nat) : Nat := 1 + x1 + x2
let f3 (x1 : Nat) (x2 : Nat) (x3 : Nat) : Nat := 1 + x1 + x2 + x3
let f4 (x1 : Nat) (x2 : Nat) (x3 : Nat) (x4 : Nat) : Nat := 1 + x1 + x2 + x3 + x4
let f5 (x1 : Nat) (x2 : Nat) (x3 : Nat) (x4 : Nat) (x5 : Nat) : Nat := 1 + x1 + x2 + x3 + x4 + x5
f5 2 3 4 5 6 + f4 2 3 4 5 + f3 2 3 4 + f2 2 3 + f1 2