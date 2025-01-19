module LetExample where 
open import Agda.Builtin.IO  
open import Agda.Builtin.Nat

n : Nat
n = let
    x1 = 1
in
    let
    x2 = x1
in
    let
    x3 = x2
in
    x3