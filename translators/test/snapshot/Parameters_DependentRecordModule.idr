module Main
import Data.Vect

record X (f1 : Nat) (f2 : Nat) (f3 : Nat) (f4 : Nat) (f5 : Nat) where
    constructor Const
    sums : Nat
    values : List Nat

example : X 1 2 3 4 5
example = Const (1 + 2 + 3 + 4 + 5)  [1, 2, 3, 4, 5]

main : IO()
main = putStrLn ""