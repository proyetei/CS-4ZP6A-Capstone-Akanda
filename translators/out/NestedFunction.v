Module NestedFunction.


Definition n : nat := let f1 (x1 : nat) : nat := x1 + 1 in
    let f2 (x1 : nat) (x2 : nat) : nat := x2 + x1 + 1 in
    let f3 (x1 : nat) (x2 : nat) (x3 : nat) : nat := x3 + x2 + x1 + 1 in
    f1 2 + f2 2 3 + f3 2 3 4. 
Compute n.
End NestedFunction.