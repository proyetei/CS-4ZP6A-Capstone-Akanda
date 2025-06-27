module DataImplicitIndices where
open import Agda.Builtin.Nat

data D : Nat -> Nat -> Nat -> Nat -> Nat -> Set where
 C1 : {x1 x2 x3 x4 x5 : Nat} -> D x1 x2 x3 x4 x5
