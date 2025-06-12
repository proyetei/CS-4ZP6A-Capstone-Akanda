Require Import Coq.Vectors.Vector. 
Require Import List. 
Import VectorNotations. 
Import ListNotations.


Module Parameters_DependentRecordModule.


Record X (f1 : nat) (f2 : nat) (f3 : nat) (f4 : nat) (f5 : nat) : Type := Const {
  sums : nat;
  values : list nat;
}.

Definition example : X 1 2 3 4 5 :=
  Const 1 2 3 4 5 (1 + 2 + 3 + 4 + 5)  [1; 2; 3; 4; 5].

End Parameters_DependentRecordModule.