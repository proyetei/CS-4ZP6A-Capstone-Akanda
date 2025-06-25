
inductive D : Nat -> Nat -> Nat -> Nat -> Nat -> Type where
| C1 : {x1 : Nat} -> D X1 0 0 0 0
| C2 : {x1 x2 : Nat} -> D X1 X2 0 0 0
| C3 : {x1 x2 x3 : Nat} -> D X1 X2 X3 0 0
| C4 : {x1 x2 x3 x4 : Nat} -> D X1 X2 X3 X4 0
| C5 : {x1 x2 x3 x4 x5 : Nat} -> D X1 X2 X3 X4 X5