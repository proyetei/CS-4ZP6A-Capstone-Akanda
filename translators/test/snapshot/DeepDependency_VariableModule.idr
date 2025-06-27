module Main

x1L1 : Nat
x1L1 = 1

x1L2 : Nat
x1L2 = 2

x1L3 : Nat
x1L3 = 3

x1L4 : Nat
x1L4 = 4

x1L5 : Nat
x1L5 = 5

x1L6 : Nat
x1L6 = 6

x1L7 : Nat
x1L7 = 7

x1L8 : Nat
x1L8 = 8

x1L9 : Nat
x1L9 = 9

x1L10 : Nat
x1L10 = 10

x2L1 : Nat
x2L1 = x1L1 + 1

x2L2 : Nat
x2L2 = x1L2 + 2

x2L3 : Nat
x2L3 = x1L3 + 3

x2L4 : Nat
x2L4 = x1L4 + 4

x2L5 : Nat
x2L5 = x1L5 + 5

x2L6 : Nat
x2L6 = x1L6 + 6

x2L7 : Nat
x2L7 = x1L7 + 7

x2L8 : Nat
x2L8 = x1L8 + 8

x2L9 : Nat
x2L9 = x1L9 + 9

x2L10 : Nat
x2L10 = x1L10 + 10

x3L1 : Nat
x3L1 = x2L1 + 1

x3L2 : Nat
x3L2 = x2L2 + 2

x3L3 : Nat
x3L3 = x2L3 + 3

x3L4 : Nat
x3L4 = x2L4 + 4

x3L5 : Nat
x3L5 = x2L5 + 5

x3L6 : Nat
x3L6 = x2L6 + 6

x3L7 : Nat
x3L7 = x2L7 + 7

x3L8 : Nat
x3L8 = x2L8 + 8

x3L9 : Nat
x3L9 = x2L9 + 9

x3L10 : Nat
x3L10 = x2L10 + 10

x4L1 : Nat
x4L1 = x3L1 + 1

x4L2 : Nat
x4L2 = x3L2 + 2

x4L3 : Nat
x4L3 = x3L3 + 3

x4L4 : Nat
x4L4 = x3L4 + 4

x4L5 : Nat
x4L5 = x3L5 + 5

x4L6 : Nat
x4L6 = x3L6 + 6

x4L7 : Nat
x4L7 = x3L7 + 7

x4L8 : Nat
x4L8 = x3L8 + 8

x4L9 : Nat
x4L9 = x3L9 + 9

x4L10 : Nat
x4L10 = x3L10 + 10

x5L1 : Nat
x5L1 = x4L1 + 1

x5L2 : Nat
x5L2 = x4L2 + 2

x5L3 : Nat
x5L3 = x4L3 + 3

x5L4 : Nat
x5L4 = x4L4 + 4

x5L5 : Nat
x5L5 = x4L5 + 5

x5L6 : Nat
x5L6 = x4L6 + 6

x5L7 : Nat
x5L7 = x4L7 + 7

x5L8 : Nat
x5L8 = x4L8 + 8

x5L9 : Nat
x5L9 = x4L9 + 9

x5L10 : Nat
x5L10 = x4L10 + 10

result : Nat
result = 100 + x5L1 + x5L2 + x5L3 + x5L4 + x5L5 + x5L6 + x5L7 + x5L8 + x5L9 + x5L10

main : IO()
main = putStrLn ""