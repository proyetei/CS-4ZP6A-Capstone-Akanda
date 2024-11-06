-- The following file proves some basic theorems in Agda.
module Basics where

{- Natural numbers -}

-- We define the (unary) natural numbers as a datatype with two constructors.
data ℕ : Set where
  zero : ℕ
  succ : ℕ → ℕ

-- In Agda, functions are defined by pattern matching and recursion.
-- We write `_+_` to say that we are defining an infix function.
_+_ : ℕ → ℕ → ℕ
zero + y = y
succ x + y = succ (x + y)

-- Set the fixity of _+_: this means that x + y + z parses as (x + y) + z
infixl 20 _+_

-- Note that we can set the fixity of functions before we define them.
-- 20 ≤ 30, so multiplication binds tighter than addition
-- (EG: x + y * z parses as x + (y * z))
infixl 30 _*_

-- Here's another binary function.
_*_ : ℕ → ℕ → ℕ
zero * y = zero
succ x * y = y + x * y

{- Vectors -}

-- It is time to see our first dependent type! The following type
-- defines the type of "vectors", EG: lists of a given length 'n'.
data Vec (A : Set) : ℕ → Set where
  []  : Vec A zero
  -- ^ The empty vector has length 0
  _∷_ : ∀ {n} → A → Vec A n → Vec A (succ n)
  -- ^ If we add an element to the front of a list, it increases the length by one

-- This is a classic example of dependent types: concatenating two
-- vectors of length `m` and `n` resp. yields a vector of length `m + n`.
_++_ : ∀ {A m n} → Vec A m → Vec A n → Vec A (m + n)
[] ++ ys = ys
(x ∷ xs) ++ ys = x ∷ (xs ++ ys)

{- Equality -}

-- Agda has no built-in notion of equality beyond computation[^1], but
-- it is strong enough to be able to define a notion of equality just by
-- using dependent types!
-- [^1]: You may see this called "definitional" or "judgemental" equality.

-- This type is quite confusing, but it follows the same pattern as `Vec`
-- from above: the idea is that we can only construct an element of `a ≡ b`
-- by using `refl`, which only works if Agda can determine if `a` *is literally the same thing*
-- as `b`, up to computation.
data _≡_ {A : Set} (a : A) : A → Set where
  refl : a ≡ a

infix 10 _≡_

-- Equality is preserved by every function.
-- Note that we prove this by pattern matching on `p : x ≡ y`: this
-- tells Agda that `x` and `y` are literally the same thing, which
-- in turn means that `f x` and `f y` are also identical, so we can
-- prove our goal with `refl`.
cong : ∀ {A B : Set} {x y : A} → (f : A → B) → x ≡ y → f x ≡ f y
cong f refl = refl

-- Now for some proofs! Let's start with an easy one: `zero + x ≡ x.
-- We can prove this via `refl`, as we literally defined `zero + x` to
-- be `x` earlier.
+-idl : ∀ (x : ℕ) → zero + x ≡ x
+-idl x = refl

-- Now for a harder one: `x + zero ≡ x`. This one does not hold by
-- definition, so we need to pattern match on `x` to get `x + zero` to compute.
+-idr : ∀ (x : ℕ) → x + zero ≡ x
+-idr zero = refl
-- ^ Here, `zero + zero` is `zero` by definition, so `refl suffices`.
+-idr (succ x) = cong succ (+-idr x)
-- ^ Here, `succ x + zero` computes to `succ (x + zero)`, so our
-- goal is to prove that `succ (x + zero) ≡ succ x`. We can use `cong`
-- to reduce the problem to showing `x + zero ≡ x`, which we can obtain
-- from a recursive call to +-idr.

-- 🏋️ Exercise: prove `+-assoc`.
-- Note that the `?` is a "hole": these are an important tool to use
-- while developing Agda code; see the resources linked in the README
-- for how to interact with them.
+-assoc : ∀ (x y z : ℕ) → x + (y + z) ≡ (x + y) + z
+-assoc x y z = {!!}

-- 🏋️ Exercise: prove `trans`.
trans : ∀ {A : Set} {x y z : A} → x ≡ y → y ≡ z → x ≡ z
trans p q = {!!}
