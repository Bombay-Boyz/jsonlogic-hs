{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell   #-}

-- |
-- Module      : JsonLogic.Core.Rule
--
-- The base functor for a JsonLogic rule tree, and its recursive closure.
--
-- __Design note (Standard 4.1 / 4.7):__ recursion over 'Rule' is never
-- hand-written. 'Fix', @cata@, @ana@, and @hylo@ are the proven,
-- well-maintained @recursion-schemes@ implementations of exactly this
-- shape — depending on them is the correction this project made to its
-- own v1 draft, not a stylistic preference.
--
-- __Design note (Standard 1.2):__ every constructor below is shaped so
-- that an arity the JsonLogic spec does not allow is a parse-time
-- impossibility, not a runtime check, /wherever the true arity is fixed
-- or a small closed range/. For the genuinely unbounded-arity operators
-- (@and@, @or@, @+@, @*@, @cat@, @merge@, @missing@), the empty-argument
-- case was checked directly against the reference implementation's
-- source rather than guessed — see each constructor's Haddock and the
-- confirmed-findings note at the bottom of this module. Only @*@ turned
-- out to be a genuine error case in the reference; the rest have a
-- clean, defined answer and are plain lists, not 'NonEmpty'.
--
-- __Design note (Standard 0.2):__ the builtin operator set is
-- closed-by-design — this sum type, matched exhaustively. Third-party,
-- user-supplied operators are the deliberate open-by-design extension
-- point, represented by exactly one constructor ('CustomOp') whose
-- behaviour is supplied externally via a Handle at evaluation time, not
-- by adding a catch-all case that blends the two disciplines.
--
-- __Design note (Standard 4.7 / 2.11), found on self-audit:__ a plain
-- @deriving Show@ on 'RuleF' gives @Show (RuleF a)@, not @Show (Fix
-- RuleF)@ — the type actually used everywhere as 'Rule'. The latter
-- needs a 'Show1'/'Eq1' instance (from "Data.Functor.Classes"), which
-- @recursion-schemes@ itself relies on to give 'Fix' its own 'Show'/'Eq'.
-- Stock deriving cannot produce these; @deriving-compat@'s
-- Template-Haskell derivers can, correctly, and are used below rather
-- than hand-writing the recursive knot.
module JsonLogic.Core.Rule
  ( RuleF (..)
  , Rule
  , OperatorName (..)
  ) where

import Data.Aeson (Value)
import Data.Eq.Deriving (deriveEq1)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Fix (Fix)
import Text.Show.Deriving (deriveShow1)

-- | A builtin or custom operator's name, as it appears as the sole key
-- of a rule object (Standard 3.2 — not a bare 'Text', so it can't be
-- confused with a variable path or any other domain string).
newtype OperatorName = OperatorName Text
  deriving (Eq, Ord, Show)

-- | One layer of a JsonLogic rule tree, parameterised over the
-- recursive position @a@. See 'Rule' for the closed, recursive type.
--
-- Every constructor's Haddock states which part of the spec it
-- implements — the invariant, not a restatement of the field names
-- (Standard 4.6).
data RuleF a
  = -- | A JSON literal that is not itself a rule — evaluates to itself.
    Lit Value
  | -- | @{"var": [path, default]}@. @path@ is itself a sub-rule (it may
    -- be computed, e.g. via 'Cat'), evaluated first to a 'Value' that is
    -- then coerced to a string or numeric index at evaluation time —
    -- this is why the path position is @a@ and not a static path type.
    -- The default is used when the lookup misses.
    Var a (Maybe a)
  | -- | @{"missing": [...]}@ — sub-rules producing the key names to
    -- check; result is the array of those absent from the data context.
    -- Empty form evaluates to @[]@ — a loop over zero keys never adds
    -- anything to the missing-keys accumulator. Confirmed against source.
    Missing [a]
  | -- | @{"missing_some": [minRequired, keys]}@ — @minRequired@ is a
    -- sub-rule producing a count, @keys@ a sub-rule producing an array
    -- of key names.
    MissingSome a a
  | -- | @{"if": [c1, t1, c2, t2, ..., else]}@. Modelled as a chain, not
    -- a fixed triple, because the spec's if/elseif/.../else form takes
    -- an arbitrary odd-length argument list, not just three.
    If (NonEmpty a)
  | -- | Empty form ('And []') evaluates to JSON @null@ — the reference
    -- implementation returns JS @undefined@ for this case (an
    -- uninitialised tracking variable when the loop never runs), which
    -- has no direct 'Value' equivalent; @null@ is the closest and most
    -- consistent mapping, matching how the rest of the spec represents
    -- "no value" (e.g. @var@'s default). Confirmed against source, not
    -- guessed — see the note at the bottom of this module.
    And [a]
  | Or [a]
  | -- | @{"!": [x]}@ — logical negation.
    Not a
  | -- | @{"!!": [x]}@ — double negation, i.e. boolean cast under the
    -- spec's truthy/falsy table (implemented as the plain, unabstracted
    -- function called for in the vision document's §4 — this
    -- constructor only names the operator, it does not encode the
    -- coercion table itself).
    Cast a
  | LooseEq a a
  | StrictEq a a
  | LooseNeq a a
  | StrictNeq a a
  | -- | @a < b@, with an optional chained @b < c@ (the spec's
    -- three-argument "is b between a and c" form). The 'Maybe' is what
    -- makes a fourth operand, or a zero/one-operand call, a parse-time
    -- impossibility rather than a case the evaluator has to reject.
    Lt a a (Maybe a)
  | Lte a a (Maybe a)
  | Gt a a (Maybe a)
  | Gte a a (Maybe a)
  | Max (NonEmpty a)
  | Min (NonEmpty a)
  | -- | Empty form evaluates to @0@ — the reference fold has an
    -- explicit zero seed. Confirmed against source: a plain list is the
    -- correct shape here, not 'NonEmpty'.
    Add [a]
  | -- | @{"-": [x]}@ negates; @{"-": [x, y, ...]}@ subtracts
    -- left-to-right. One constructor for both, per the spec's own
    -- overload — the arity, not the constructor, decides which.
    Sub (NonEmpty a)
  | -- | Empty form is a genuine runtime error in the reference
    -- implementation — the fold has no seed. 'NonEmpty' is therefore the
    -- confirmed-correct shape, not a guess: an empty 'Mul' is invalid
    -- input in the source of truth itself, so rejecting it at parse
    -- time is the more faithful match, not a stricter one.
    Mul (NonEmpty a)
  | -- | Strictly binary per spec — no chained division.
    Div a a
  | -- | Strictly binary per spec — modulo.
    Mod a a
  | Map a a
  | Filter a a
  | -- | @{"reduce": [array, iteratee, initial]}@ — always exactly three.
    Reduce a a a
  | All a a
  | None a a
  | Some a a
  | -- | Empty form evaluates to @[]@ — the reference fold has an
    -- explicit empty-list seed. Confirmed against source.
    Merge [a]
  | -- | @{"in": [needle, haystack]}@ — substring-in-string or
    -- value-in-array, decided by @haystack@'s runtime type, not by two
    -- separate constructors, matching the spec's own single operator.
    In a a
  | Cat [a]
  | -- | @{"substr": [string, start, length]}@ — @length@ is optional.
    Substr a a (Maybe a)
  | -- | Logs its evaluated argument for debugging and returns it
    -- unchanged.
    Log a
  | -- | The deliberate open extension point (Standard 0.2, 0.5) — any
    -- operator name outside the closed set above. Evaluated by handing
    -- the name and evaluated arguments to the caller-supplied Handle;
    -- this constructor carries no builtin behaviour of its own.
    CustomOp OperatorName [a]
  deriving (Functor, Foldable, Traversable, Eq, Show)

$(deriveEq1 ''RuleF)
$(deriveShow1 ''RuleF)

-- | The closed, recursive JsonLogic rule tree — @Fix RuleF@, per the
-- vision document's corrected §4: 'Fix' and its 'Data.Functor.Foldable'
-- combinators come from @recursion-schemes@, not a hand-rolled
-- equivalent. 'Eq'/'Show' on 'Rule' itself come from @recursion-schemes@'s
-- @Eq1 f => Eq (Fix f)@ / @Show1 f => Show (Fix f)@ instances, which the
-- two splices above make available.
type Rule = Fix RuleF

-- ---------------------------------------------------------------------
-- CONFIRMED FINDINGS (this session) — supersedes the earlier "reasoned
-- default, provisional" note. Checked directly against
-- jwadhams/json-logic-js's logic.js, commit c5c73601, not inferred:
--
--   +       empty -> 0            (explicit zero seed on the fold)
--   *       empty -> RUNTIME ERROR (fold has no seed)  => kept NonEmpty
--   cat     empty -> ""           (string-join over zero items)
--   merge   empty -> []           (explicit empty-list seed on the fold)
--   missing empty -> []           (loop over zero keys adds nothing)
--   and     empty -> JS `undefined` (uninitialised tracking variable)
--   or      empty -> JS `undefined`, same mechanism
--
-- `undefined` has no direct 'Value' equivalent; it is mapped to JSON
-- @null@ here as the closest, most consistent choice, matching how the
-- rest of the spec represents "no value" elsewhere (e.g. @var@'s
-- default). This is a recorded, deliberate mapping decision (Standard
-- 5.9), not an unexamined one.
--
-- Net effect: 'Mul' is the only constructor above where 'NonEmpty' is
-- actually confirmed-correct against the reference; 'And', 'Or', 'Add',
-- 'Cat', 'Merge', and 'Missing' are plain lists, matching defined,
-- non-error reference behaviour.
-- ---------------------------------------------------------------------
