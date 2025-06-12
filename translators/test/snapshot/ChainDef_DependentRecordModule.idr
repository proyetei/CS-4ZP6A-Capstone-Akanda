module Main
import Data.Vect

record Record1 where
    constructor Const1
    f1 : Nat

record Record2 where
    constructor Const2
    f2 : Record1

record Record3 where
    constructor Const3
    f3 : Record2

record Record4 where
    constructor Const4
    f4 : Record3

record Record5 where
    constructor Const5
    f5 : Record4

example : Record5
example =  Const5 (Const4 (Const3 (Const2 (Const1 10))))

main : IO()
main = putStrLn ""