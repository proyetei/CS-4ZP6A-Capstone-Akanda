
Require Import Coq.Vectors.Vector.
Import VectorNotations.

Module Fields_DependentRecordModule.

Record Cap_X : Type := Const {
  f1 : nat;
  f2 : Vect nat f1;
  f3 : Vect nat (S f1);
  f4 : Vect nat (S (S f1));
  f5 : Vect nat (S (S (S f1)));
}.

Definition example : Cap_X :=
  Const [1] [1; 1] [1; 1; 1] [1; 1; 1; 1] [1; 1; 1; 1; 1].

End Fields_DependentRecordModule.