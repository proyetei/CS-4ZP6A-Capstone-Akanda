data Natural : Type where
  Zero : Natural
  Succ : Natural -> Natural


(+) : Natural -> Natural -> Natural
Zero + y = y
(Succ x) + y = Succ (x + y)

infixl 8 +

(*) : Natural -> Natural -> Natural
Zero * y = Zero
(Succ x) * y = y + (x * y)

data Vec : (a : Type) -> Natural -> Type where
  Nil : Vec a Zero
  Cons : {n : Natural} -> a -> Vec a n -> Vec a (Succ n)

(++) : {n : Natural} -> {m : Natural} -> Vec a n -> Vec a m -> Vec a (n + m)
[] ++ ys = ys
(Cons x xs) ++ ys = Cons x (xs ++ ys)

data Eq : {a : Type} -> (x : a) -> (y : a) -> Type where
  Refl : Eq x x

cong : {x : a} -> {y : a} -> (f : a -> b) -> Eq x y -> Eq (f x) (f y)
cong f Refl = Refl

addLeftId : (n : Natural) -> Eq (Zero + n) n
addLeftId n = Refl

addRightId : (n : Natural) -> Eq (n + Zero) n
addRightId Zero = Refl
addRightId (Succ n) = cong Succ (addRightId n)

-- 🏋️ Exercise: prove addAssoc

addAssoc : (x : Natural) -> (y : Natural) -> (z : Natural) -> Eq (x + (y + z)) ((x + y) + z)
addAssoc x y z = ?_addAssoc

-- 🏋️ Exercise: prove eqTrans

eqTrans : {x : a} -> {y : a} -> {z : a} -> Eq x y -> Eq y z -> Eq x z
eqTrans p q = ?_eqTrans
