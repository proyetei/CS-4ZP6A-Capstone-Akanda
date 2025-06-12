


Module IndicesParameters_Datatypes.


Inductive d (p1: Type)  (p2: Type)  (p3: Type)  (p4: Type)  (p5: Type) : nat -> nat -> nat -> nat -> nat -> Type := 
| c : forall {x1 x2 x3 x4 x5 : nat}, d p1 p2 p3 p4 p5 x1 x2 x3 x4 x5.
End IndicesParameters_Datatypes.