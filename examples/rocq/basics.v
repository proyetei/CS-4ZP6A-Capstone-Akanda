(* The natural numbers as an inductive type. *)
Inductive Nat : Type :=
| Zero : Nat
| Succ : Nat -> Nat.

(* Addition of natural numbers *)
Fixpoint add (n m : Nat) {struct n} : Nat :=
  match n with
  | Zero => m
  | Succ p => Succ (add p m)
  end.

(* Multiplication of natural numbers *)
Fixpoint mul (n m : Nat) {struct n} : Nat :=
  match n with
  | Zero => Zero
  | Succ n => add m (mul n m)
  end.

Infix "+" := add (at level 50, left associativity).
Infix "*" := mul (at level 40, left associativity).

(* Vectors *)
Inductive Vec (A : Type) : Nat -> Type :=
| Nil : Vec A Zero
| Cons : forall {n : Nat}, A -> Vec A n -> Vec A (Succ n).

(* Concatenation of vectors. *)
Fixpoint concat {A : Type} {n m : Nat} (xs : Vec A n) (ys : Vec A m) : Vec A (n + m).
  refine
    match xs in Vec _ l return Vec _ (l + m) with
    | Nil _ => ys
    | Cons _ x xs => Cons _ x (concat _ _ _ xs ys)
    end.
Defined.

(* Like lean, rocq is tactic based. *)
Theorem cong {A B : Type} {x y : A} (f : A -> B) (p : x = y) : f x = f y.
  rewrite p.
  reflexivity.
Qed.

(* Left identity of addition holds by definition. *)
Theorem add_left_identity (n : Nat) : Zero + n = n.
    reflexivity.
Qed.

(* Right identity of addition holds by definition. *)
Theorem add_right_identity (n : Nat) : n + Zero = n.
  induction n as [| n ih ].
  - reflexivity.
  - simpl.
    rewrite ih.
    reflexivity.
Qed.

(* 🏋️ Exercise: prove `add_assoc` *)
Theorem add_assoc (x y z : Nat) : x + (y + z) = (x + y) + z.
Admitted.

(* 🏋️ Exercise: prove `eq_trans` *)
Theorem eq_trans {A : Type} {x y z : A} (p : x = y) (q : y = z) : x = z.
Admitted.
