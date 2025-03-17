module LetExample where 
open import Agda.Builtin.IO  
open import Agda.Builtin.Nat 
open import Data.Vec 
open import Agda.Builtin.List 

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
    let
    x4 = x3
 in
    let
    x5 = x4
 in
    x5
