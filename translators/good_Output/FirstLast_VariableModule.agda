module FirstLast_VariableModule where 
open import Agda.Builtin.IO  
open import Agda.Builtin.Nat 
open import Data.Vec 
open import Agda.Builtin.List 

x1 : Nat
x1 = 1
x2 : Nat
x2 = 2
x3 : Nat
x3 = 3
x4 : Nat
x4 = 4
x5 : Nat
x5 = 5
result : Nat
result = x1 + x5
