module NestedFunction where 
open import Agda.Builtin.IO  
open import Agda.Builtin.Nat

n : Nat
n = let
    f1 : Nat -> Nat
    f1 x1 = x1 + 1
    f2 : Nat -> Nat -> Nat
    f2 x1 x2 = x2 + x1 + 1
    f3 : Nat -> Nat -> Nat -> Nat
    f3 x1 x2 x3 = x3 + x2 + x1 + 1
    in f1 2 + f2 2 3 + f3 2 3 4