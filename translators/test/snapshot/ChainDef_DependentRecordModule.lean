


structure Record1 where
    Const1 ::
    f1 : Nat

structure Record2 where
    Const2 ::
    f2 : Record1

structure Record3 where
    Const3 ::
    f3 : Record2

structure Record4 where
    Const4 ::
    f4 : Record3

structure Record5 where
    Const5 ::
    f5 : Record4

open Record1 Record2 Record3 Record4 Record5
example : Record5 := Const5 (Const4 (Const3 (Const2 (Const1 10))))