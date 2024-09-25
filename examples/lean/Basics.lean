/- The following file serves as a (very) short introduction to lean. -/
namespace Basic

/- Natural numbers -/

-- The unary natural numbers as a datatype.
inductive ℕ where
| zero : ℕ
| succ : ℕ → ℕ

-- Addition of unary natural numbers via pattern matching.
def add : ℕ → ℕ → ℕ
| .zero, y => y
| .succ x, y => .succ (add x y)

-- Notation for `add`.
infixr:60 " + " => add

-- Multiplication of unary natural numbers via pattern matching.
def mul : ℕ → ℕ → ℕ
| .zero, _ => .zero
| .succ x, y => y + (mul x y)

-- Notation for `mul`.
infixr:70 " * " => mul

/- Vectors -/
inductive Vec (α : Type) : ℕ → Type where
| nil : Vec α .zero
| cons : {n : ℕ} → α → Vec α n → Vec α (.succ n)

-- Vector concatenation
def Vec.concat {m n : ℕ} : Vec α m → Vec α n → Vec α (m + n)
| .nil, ys => ys
| .cons x xs, ys => .cons x (Vec.concat xs ys)

-- We skip defining the equality type in Lean, as it comes with it's
-- own built-in equality, written `x = y`.

-- So far, things have looked a lot like Agda. However, that is about to
-- change! Lean is designed around "tactics", which are metaprograms that
-- produce proofs when you run them. Here, we use the `rw` tactic to rewrite
-- all occurances of `x = y` in the goal; this completes the proof.
-- A longer proof that follows the Agda one would be `induction p; trivial`,
-- which does induction over the identity proof, and then discharges the now
-- trivial goal of `f x = f x`.
theorem cong {x y : α} (f : α → β) (p : x = y) : f x = f y := by rw [p]

-- Like the other examples, this theorem holds by definition.
theorem add_left_identity (x : ℕ) : ℕ.zero + x = x := by rfl

-- To prove right identity, we need to do induction on `x`.
-- The `simp[add]` tells lean to unfold the definition of `add (succ x) y` while
-- automatically simplifying the goal
theorem add_right_identity (x : ℕ) : x + ℕ.zero = x := by
    induction x with
    | zero => rfl
    | succ x ih => simp[add]; exact ih

-- 🏋️ Exercise: prove `add_assoc`
theorem add_assoc (x y z : ℕ) : x + (y + z) = (x + y) + z := by
  sorry

theorem eq_trans {x y z : α} (p : x = y) (q : y = z) : x = z := by
  sorry
