module IndicesParameters_Datatypes where
open import Agda.Builtin.Nat

data D (p1 : Set) (p2 : Set) (p3 : Set) (p4 : Set) (p5 : Set) : Nat -> Nat -> Nat -> Nat -> Nat -> Set where
 C : {X1 X2 X3 X4 X5 : Nat} -> D p1 p2 p3 p4 p5 X1 X2 X3 X4 X5
