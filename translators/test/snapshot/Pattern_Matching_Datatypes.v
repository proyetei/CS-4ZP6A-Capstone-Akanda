

Module Pattern_Matching_Datatypes.

Inductive d : Type :=
| c1 : d
| c2 : d
| c3 : d
| c4 : d
| c5 : d.

Fixpoint F (C : d) : nat :=
match C with
| c1 => 1
| c2 => 2
| c3 => 3
| c4 => 4
| c5 => 5end.

Definition N : nat := F c5 + F c4 + F c3 + F c2 + F c1.

End Pattern_Matching_Datatypes.