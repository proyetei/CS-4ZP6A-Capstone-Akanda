module Main

record Cap_X where
    constructor Const
    f1 : Nat
    f2 : Nat
    f3 : Nat
    f4 : Nat
    f5 : Nat

example : Cap_X
example = Const 1 1 1 1 1

main : IO()
main = putStrLn ""