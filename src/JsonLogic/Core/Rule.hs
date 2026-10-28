{-# LANGUAGE DeriveTraversable #-}

-- |
-- Module      : JsonLogic.Core.Rule
--
-- The rule tree: one layer of it ('RuleF', the base functor), and the
-- recursive tree whose every node records where in the rule's JSON it came
-- from ('Rule').
--
-- Recursion over the tree is never written by hand: the parser builds it
-- with an unfold ('Data.Fix.unfoldFixM') and the evaluator consumes it with a
-- fold ('Data.Fix.foldFix'), both from @data-fix@.
--
-- The builtin operators are a closed sum, matched exhaustively, so an
-- operator cannot be half-implemented. Operators supplied by the caller are
-- the one open extension point, 'CustomOp', whose behaviour is looked up when
-- the rule is compiled.
--
-- Constructors are shaped so that, where an operator takes a fixed number
-- of operands or a small range of them, a wrong count cannot be built
-- ('Lt' takes two operands and an optional third, for instance). The
-- operators that take any number of operands hold a list.
module JsonLogic.Core.Rule
  ( RuleF (..)
  , Node (..)
  , Rule
  ) where

import Data.Aeson (Value)
import Data.Fix (Fix (..))
import Data.Foldable (toList)
import Data.Functor (void)
import Data.Functor.Classes (Eq1 (..), Show1 (..))

import JsonLogic.Core.Location (Location)
import JsonLogic.Core.Operator (OperatorName)

-- | One layer of a JsonLogic rule, with its sub-rules at type @a@. Each
-- constructor's comment states what the reference implementation does, which
-- is what the evaluator implements.
data RuleF a
  = -- | A JSON scalar, or an object that is not an operator call (it has zero
    -- or several keys). Evaluates to itself.
    Lit Value
  | -- | A JSON array. The reference evaluates an array element by element,
    -- each element being a rule.
    Arr [a]
  | -- | @{"var": [path, default]}@. The path is a sub-rule, evaluated and
    -- then read as a dotted path or an index; the default, if given, is
    -- used when the lookup finds nothing.
    Var a (Maybe a)
  | -- | @{"missing": [k1, k2, ...]}@: the keys whose values are absent, or
    -- empty strings. If the first key evaluates to an array, that array is
    -- the key list.
    Missing [a]
  | -- | @{"missing_some": [need, keys]}@: nothing, if at least @need@ of the
    -- keys are present; otherwise the missing ones.
    MissingSome a a
  | -- | @{"if": [c1, t1, c2, t2, ..., else]}@ (also written @?:@): the
    -- consequent of the first truthy condition, else the final operand if the
    -- count is odd, else @null@. Only what is needed is evaluated.
    If [a]
  | -- | The first falsy operand, or the last operand; JavaScript @undefined@
    -- when there are none. Stops at the first falsy one.
    And [a]
  | -- | The first truthy operand, or the last operand; JavaScript
    -- @undefined@ when there are none. Stops at the first truthy one.
    Or [a]
  | -- | @!@: the negation of the operand's truthiness.
    Not a
  | -- | @!!@: the operand's truthiness.
    DoubleNot a
  | -- | @==@, JavaScript's loose equality.
    LooseEq a a
  | -- | @===@, JavaScript's strict equality.
    StrictEq a a
  | -- | @!=@.
    LooseNeq a a
  | -- | @!==@.
    StrictNeq a a
  | -- | @<@, or with a third operand the chained @a < b < c@ ("between").
    Lt a a (Maybe a)
  | -- | @<=@, or with a third operand the chained @a <= b <= c@.
    Lte a a (Maybe a)
  | -- | @>@, strictly binary.
    Gt a a
  | -- | @>=@, strictly binary.
    Gte a a
  | -- | @Math.max@ of the operands: @-Infinity@ (written @null@) for none.
    Max [a]
  | -- | @Math.min@ of the operands: @Infinity@ (written @null@) for none.
    Min [a]
  | -- | The sum of the operands read with @parseFloat@; @0@ for none.
    Add [a]
  | -- | @{"-": [x]}@ negates; @{"-": [x, y]}@ subtracts.
    Sub a (Maybe a)
  | -- | The product. The reference folds without a seed: one operand is
    -- returned as it is, and none at all throws (@InvalidOperand@
    -- when reached; strict mode refuses to read it).
    Mul [a]
  | -- | @/@, strictly binary.
    Div a a
  | -- | @%@, JavaScript's truncating remainder, strictly binary.
    Mod a a
  | -- | @{"map": [array, body]}@: the body evaluated against each element.
    Map a a
  | -- | @{"filter": [array, body]}@: the elements for which the body is
    -- truthy.
    Filter a a
  | -- | @{"reduce": [array, body, initial]}@: the body evaluated against
    -- @{"current": element, "accumulator": so far}@ for each element.
    Reduce a a a
  | -- | Whether the body is truthy for every element (false for none).
    All a a
  | -- | Whether the body is truthy for no element (true for none).
    None a a
  | -- | Whether the body is truthy for some element (false for none).
    Some a a
  | -- | The operands concatenated, arrays flattened by one level.
    Merge [a]
  | -- | @{"in": [needle, haystack]}@: substring of a string, or strict-equal
    -- element of an array.
    In a a
  | -- | The operands joined as strings.
    Cat [a]
  | -- | @{"substr": [string, start, length]}@, the length optional.
    Substr a a (Maybe a)
  | -- | @log@: its operand, unchanged. The reference also prints it; this
    -- library does no I/O.
    Log a
  | -- | @Discard inner extras@: the value of @inner@, after evaluating each
    -- of @extras@ for its failures only. Built in lenient arity mode only,
    -- for the surplus operands of an operator the reference evaluates in
    -- full before applying it, so that a surplus operand that fails still
    -- fails the rule, as in the reference.
    Discard a [a]
  | -- | An operator outside the builtin set, applied to its operands. Its
    -- behaviour is supplied by the caller.
    CustomOp OperatorName [a]
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Two layers are equal when they have the same shape (constructor, fields
-- other than sub-rules, and number of sub-rules) and pairwise-equal
-- sub-rules. Written with 'Foldable' instead of derived by Template Haskell:
-- 'void' keeps the shape, 'toList' gives the sub-rules in order.
instance Eq1 RuleF where
  liftEq eq x y = void x == void y && and (zipWith eq (toList x) (toList y))

-- | The derived 'Show', with each sub-rule shown by the given function.
instance Show1 RuleF where
  liftShowsPrec showsChild _ d = showsPrec d . fmap (Shown . flip showsChild)

-- | A layer of the tree and where it came from.
data Node a = Node
  { nodeLocation :: !Location
  -- ^ The JSON Pointer of this node in the rule.
  , nodeRule :: !(RuleF a)
  -- ^ The node itself.
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Eq1 Node where
  liftEq eq (Node a x) (Node b y) = a == b && liftEq eq x y

instance Show1 Node where
  liftShowsPrec showsChild _ d = showsPrec d . fmap (Shown . flip showsChild)

-- | A rule as read from JSON: every node carries its 'Location'.
type Rule = Fix Node

-- | A value shown by a function, so that a derived 'Show' can be reused to
-- write 'Show1' instances.
newtype Shown = Shown (Int -> ShowS)

instance Show Shown where
  showsPrec d (Shown shows') = shows' d
