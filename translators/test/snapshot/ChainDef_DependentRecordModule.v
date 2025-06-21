

Module ChainDef_DependentRecordModule.

Record Record1 : Type := Const1 {
  f1 : nat;
}.

Record Record2 : Type := Const2 {
  f2 : Record1;
}.

Record Record3 : Type := Const3 {
  f3 : Record2;
}.

Record Record4 : Type := Const4 {
  f4 : Record3;
}.

Record Record5 : Type := Const5 {
  f5 : Record4;
}.

Definition example : Record5 := 
  Const5 (Const4 (Const3 (Const2 (Const1 10)))).

End ChainDef_DependentRecordModule.