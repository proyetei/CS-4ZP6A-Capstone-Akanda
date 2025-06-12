


structure Record1 where
    Const1 ::
    f1 : Nat

structure Record2 where
    Const2 ::
    f2 : Nat

structure Record3 where
    Const3 ::
    f3 : Nat

structure Record4 where
    Const4 ::
    f4 : Nat

structure Record5 where
    Const5 ::
    f5 : Nat

open Record1 Record2 Record3 Record4 Record5
example : Record5 := Const5 1