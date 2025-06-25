
inductive D (p1 : Type) (p2 : Type) (p3 : Type) (p4 : Type) (p5 : Type) : Nat -> Nat -> Nat -> Nat -> Nat -> Type where
| C : {X1 X2 X3 X4 X5 : Nat} -> D p1 p2 p3 p4 p5 X1 X2 X3 X4 X5