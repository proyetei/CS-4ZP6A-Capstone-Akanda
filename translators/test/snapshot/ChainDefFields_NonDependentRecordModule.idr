module Main
import Data.Vect

record Record1 where
    constructor Const1
    f1 : Nat

record Record2 where
    constructor Const2
    f2 : Nat

record Record3 where
    constructor Const3
    f3 : Nat

record Record4 where
    constructor Const4
    f4 : Nat

record Record5 where
    constructor Const5
    f5 : Nat

example : Record5
example = Const 1

main : IO()
main = putStrLn ""