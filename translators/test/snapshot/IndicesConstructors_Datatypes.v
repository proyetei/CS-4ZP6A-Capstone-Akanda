

Module IndicesConstructors_Datatypes.

Inductive d : nat -> nat -> nat -> nat -> nat -> Type := 
| c1 : forall {x1 : nat}, d x1 0 0 0 0 
| c2 : forall {x1 x2 : nat}, d x1 x2 0 0 0 
| c3 : forall {x1 x2 x3 : nat}, d x1 x2 x3 0 0 
| c4 : forall {x1 x2 x3 x4 : nat}, d x1 x2 x3 x4 0 
| c5 : forall {x1 x2 x3 x4 x5 : nat}, d x1 x2 x3 x4 x5.
End IndicesConstructors_Datatypes.