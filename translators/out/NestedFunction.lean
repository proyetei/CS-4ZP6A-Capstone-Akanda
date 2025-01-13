

def n : Nat := 
let f1 (x1 : Nat) : Nat := x1 + 1
let f2 (x1 : Nat) (x2 : Nat) : Nat := x2 + x1 + 1
let f3 (x1 : Nat) (x2 : Nat) (x3 : Nat) : Nat := x3 + x2 + x1 + 1
f1 2 + f2 2 3 + f3 2 3 4