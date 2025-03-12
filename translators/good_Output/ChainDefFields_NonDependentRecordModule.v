Require Import Coq.Vectors.Vector. 
Require Import List. 
Import VectorNotations. 
Import ListNotations.


Module ChainDefFields_NonDependentRecordModule.


Record Record1 : Type := Const1 {
  f1 : nat;
}.

Record Record2 : Type := Const2 {
  f2 : nat;
}.

Record Record3 : Type := Const3 {
  f3 : nat;
}.

Record Record4 : Type := Const4 {
  f4 : nat;
}.

Record Record5 : Type := Const5 {
  f5 : nat;
}.

Definition example : Record5 :=
  Const 1.

End ChainDefFields_NonDependentRecordModule.