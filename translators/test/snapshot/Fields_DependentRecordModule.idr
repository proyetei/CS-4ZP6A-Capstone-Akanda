module Main
import Data.Vect

record Cap_X where
    constructor Const
    f1 : Nat
    f2 : Vect f1 Nat
    f3 : Vect (S f1) Nat
    f4 : Vect (S (S f1)) Nat
    f5 : Vect (S (S (S f1))) Nat

example : Cap_X
example = Const [1] [1, 1] [1, 1, 1] [1, 1, 1, 1] [1, 1, 1, 1, 1]

main : IO()
main = putStrLn ""