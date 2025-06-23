module Main

data D : (p1 : Type) -> (p2 : Type) -> (p3 : Type) -> (p4 : Type) -> (p5 : Type) -> Type where
 C1 : p1 -> p2 -> p3 -> p4 -> p5 -> D p1 p2 p3 p4 p5
 C2 : p1 -> p2 -> p3 -> p4 -> p5 -> D p1 p2 p3 p4 p5
 C3 : p1 -> p2 -> p3 -> p4 -> p5 -> D p1 p2 p3 p4 p5
 C4 : p1 -> p2 -> p3 -> p4 -> p5 -> D p1 p2 p3 p4 p5
 C5 : p1 -> p2 -> p3 -> p4 -> p5 -> D p1 p2 p3 p4 p5

main : IO()
main = putStrLn ""