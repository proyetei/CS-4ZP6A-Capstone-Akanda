Require Import Coq.Vectors.Vector. 
Require Import List. 
Import VectorNotations. 
Import ListNotations.


Module Fields_NonDependentRecordModule.


Record X : Type := Const {
  f1 : nat;
  f2 : nat;
  f3 : nat;
  f4 : nat;
  f5 : nat;
}.

Definition example : X :=
  Const 1 1 1 1 1.

End Fields_NonDependentRecordModule.