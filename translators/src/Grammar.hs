-- | The shared grammar of proof assistants based off MLTT/CIC.
module Grammar
  ( -- * Terms
    --
    -- $terms
    Tm(..)
  , ArgVisibility(..)
  , ArgInfo(..)
  , Arg(..)
  , Literal(..)
  , NotationSegment(..)
  -- * Definitions
  --
  -- $definitions
  , Def(..)
  )
  where

import Data.Text qualified as T
import GHC.Natural

import Data.List.NonEmpty
import Data.Text (Text)


-- [TODO: Reed M, 12/06/2025] Datatype for qualified names instead of just Text.

-- [TODO: Reed M, 12/06/2025] Match-expressions.
-- $terms
--
-- The terms of MLTT/CIC. As we are just trying to target
-- the concrete syntax of various proof assistants, we opt
-- to do types ala Russel, and unify types and elements of
-- the universe.
--
-- For similar reasons, we opt to use nominal variables over
-- something like De Bruijn or locally nameless. We don't ever
-- need to do substitution, and part of benchmarking is testing
-- how systems handle elaboration of shadowed names.

-- | The syntax of terms.
data Tm
  = Var Text
  -- ^ Nominal variables.
  | Lam (NonEmpty (Arg Text)) Tm
  -- ^ N-ary lambda abstraction.
  -- We use n-ary lambdas over unary ones to be faithful
  -- to the concrete syntax.
  | App Tm (NonEmpty (Arg Tm))
  -- ^ N-ary application.
  -- Using n-ary application makes it slightly easier to get good pretty printing,
  -- as we can insert break hints more consistently.
  | Notation (NonEmpty NotationSegment)
  -- ^ We need to handle notation separately from normal function application.
  -- Note that we do not include argument info, as most notation systems use
  -- normal application for implicit instantation.
  | Pi (NonEmpty (Arg (NonEmpty Text, Tm))) Tm
  -- ^ N-ary pi types.
  -- We make sure to allow for concrete syntax like @(x y z : A) -> B@.
  | Let (NonEmpty (Arg (Text, Tm))) Tm
  -- ^ N-ary let bindings.
  -- A single @let@ expression with multiple bindings is translated
  -- as a block of bindings.
  | Lit Literal
  -- ^ Literals.
  | Parens Tm
  -- ^ Parenthesis.
  -- These are typically gone even by the time we hit concrete syntax
  -- in most proof assistants, but we want to keep them around to be
  -- able to test parsers.
  deriving (Show)

-- | The visibility of an argument: either implicit or visible.
data ArgVisibility = Implicit | Visible
  deriving (Show)

-- | Argument information.
data ArgInfo = ArgInfo
  { argVisibility :: ArgVisibility
  -- ^ Is a argument visible or implicit?
  }
  deriving (Show)

-- | An argument along with argument information.
-- These show up both in binders, and in applications.
data Arg a = Arg
  { unArg :: a
  -- ^ The actual argument.
  , argInfo :: ArgInfo
  -- ^ Argument information.
  }
  deriving (Show)

-- | Literals.
data Literal
  = Nat Natural
  -- ^ Natural number literals.
  -- We will attempt to translate these as literals like @100@
  -- instead of @succ@ and @zero@ constructors.
  | Bool Bool
  -- ^ Boolean literals.
  | List [Tm]
  -- ^ List literals.
  -- We will attempt to translate these as literals like @[x, y, z]@
  -- as opposed to cons constructors.
  | Vec [Tm]
  -- ^ Vector literals.
  -- We will attempt to translate these as literals like @[x, y, z]@
  -- as opposed to @cons@ and @nil@ constructors.
  deriving (Show)

-- | A segment of notation.
data NotationSegment
  = NotationOp Text
  -- ^ An operator in a piece of notation.
  -- An example of this is the @+@ in @2 + y@
  -- or @[@ in @C [ f ∘ g ]@.
  | NotationTm Tm
  -- ^ An term in a piece of notation.
  -- An example of this is the @2@ or @y@ in @2 + y@
  -- or @f@ in @C [ f ∘ g ]@.
  deriving (Show)

-- [TODO: Reed M, 12/06/2025] Datatypes.
-- [TODO: Reed M, 12/06/2025] Records.
-- [TODO: Reed M, 12/06/2025] Sections/modules.
-- [TODO: Reed M, 12/06/2025] Open expressions.
-- [TODO: Reed M, 12/06/2025] Variable blocks.
-- $definitions

-- | Definitions
data Def
  = TpAnn Text Tm
  -- ^ Top-level type annotation.
  -- Example:
  -- @foo : (x y : Nat) → Vec Bool (x + y)@
  | Fn Text (NonEmpty Clause)
  -- ^ Top-level function definition.
  deriving (Show)

-- | Top-level definition clause.
data Clause = Clause
  { clauseLhs :: ClauseLHS
  -- ^ Left-hand side of a clause.
  , clauseRhs :: ClauseRHS
  -- ^ Right-hand side of a clause
  }
  deriving (Show)

-- [TODO: Reed M, 12/06/2025] With-abstraction.
-- [TODO: Reed M, 12/06/2025] Copatterns.
data ClauseLHS
  = PatternLHS [Pattern]
  -- ^ Pattern-matching clause.
  deriving (Show)

data ClauseRHS
  = AbsurdRHS
  -- ^ The RHS of an absurd pattern.
  | TmRHS Tm
  -- ^ A RHS of a clause that is just a term.
  deriving (Show)

-- [TODO: Reed M, 12/06/2025] Investigate what the unified pattern language ought to be.
-- [TODO: Reed M, 12/06/2025] Mixfix patterns?
data Pattern
  = ConPat Text [Arg Pattern]
  -- ^ Constructor pattern.
  -- Example @foo (suc x) = ...@
  | VarPat Text
  -- ^ Variable patterns.
  | AbsurdPat
  -- ^ Absurd patterns.
  deriving (Show)
