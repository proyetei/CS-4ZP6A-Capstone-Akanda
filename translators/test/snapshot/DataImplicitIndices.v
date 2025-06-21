

Module DataImplicitIndices.

Inductive d : nat -> nat -> nat -> nat -> nat -> Type := 
| c1 : forall {x1 x2 x3 x4 x5 : nat}, d x1 x2 x3 x4 x5.
End DataImplicitIndices.