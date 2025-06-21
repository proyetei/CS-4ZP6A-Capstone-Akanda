
Require Import Coq.Vectors.Vector. 
Import VectorNotations.

Module Fields_DependentRecordModule.

Record Cap_X : Type := Const {
  f1 : nat;
  f2 : t nat f1;
  f3 : t nat (S f1);
  f4 : t nat (S (S f1));
  f5 : t nat (S (S (S f1)));
}.

Definition example : Cap_X := 
  Const [1] [1; 1] [1; 1; 1] [1; 1; 1; 1] [1; 1; 1; 1; 1].

End Fields_DependentRecordModule.