module Main

data D : Nat -> Nat -> Nat -> Nat -> Nat -> Type where
 C1 : {x1, x2, x3, x4, x5 : Nat} -> D x1 x2 x3 x4 x5

main : IO()
main = putStrLn ""