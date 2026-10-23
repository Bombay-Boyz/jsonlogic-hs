-- |
-- Module      : JsonLogic.Core.Analysis
--
-- What a rule can read from its data, found without running it.
--
-- The reference implementation offers @uses_data@, which lists every @var@
-- name in a rule, including those inside a @map@ body that read the element
-- rather than the data. This analysis follows the scopes instead: the body
-- of @map@, @filter@, @reduce@, @all@, @none@ and @some@ is evaluated against
-- an element (or, for @reduce@, against @current@ and @accumulator@), and
-- classic JsonLogic has no way to reach the outer data from there, so what a
-- body reads is part of what its collection operand reads.
--
-- The answer is an over-approximation, and exact about what it promises: if
-- 'usesAllData' is false, the result of the rule (value or error) depends on
-- the data only through the values at 'usedPaths'. A path is the dotted path
-- @var@ would follow, as written in the rule. Anything that cannot be known
-- statically (a computed path, a path that is the data itself, a key list
-- that comes from the data, a custom operator, which is handed the whole
-- datum) sets 'usesAllData'.
module JsonLogic.Core.Analysis
  ( DataUse (..)
  , dataUse
  ) where

import Data.Aeson (Value (..))
import Data.Fix (foldFix)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

import JsonLogic.Core.Rule (Node (..), Rule, RuleF (..))
import JsonLogic.Core.Value (fromValue, toJsString)

-- | What a rule can read from its data.
data DataUse = DataUse
  { usedPaths :: [Text]
  -- ^ The paths it can read, sorted and without duplicates.
  , usesAllData :: Bool
  -- ^ Whether it can read anything else, so that 'usedPaths' is not the
  -- whole story.
  }
  deriving (Eq, Show)

-- | What the rule can read, as a set while it is built.
data Reads = Reads !(Set.Set Text) !Bool

instance Semigroup Reads where
  Reads a x <> Reads b y = Reads (Set.union a b) (x || y)

instance Monoid Reads where
  mempty = Reads Set.empty False

-- | One layer's analysis: what the node and its operands read, and its
-- shape, so that a parent can see a literal path or key list.
data Layer = Layer
  { layerReads :: Reads
  , layerShape :: Shape
  }

-- | What a node is, as far as a parent's lookups care.
data Shape
  = -- | A literal: a scalar, or an object that is not an operator call.
    Literal Value
  | -- | An array literal, whose elements are each a rule.
    ArrayOf [Shape]
  | -- | Anything computed.
    Computed

-- | What a rule can read from its data.
dataUse :: Rule -> DataUse
dataUse rule = case layerReads (foldFix layer rule) of
  Reads paths whole -> DataUse (Set.toAscList paths) whole

layer :: Node Layer -> Layer
layer (Node _ node) = Layer (readsOf node) shape
  where
    shape = case node of
      Lit value -> Literal value
      Arr items -> ArrayOf (map layerShape items)
      _ -> Computed

readsOf :: RuleF Layer -> Reads
readsOf node = case node of
  Var path _ -> operands <> lookupOf (layerShape path)
  Missing keys -> operands <> missingLookups (map layerShape keys)
  MissingSome _ options -> operands <> optionLookups (layerShape options)
  -- an iteration body reads the element, which its collection already
  -- accounts for; only the collection (and reduce's initial value) are
  -- evaluated against the data
  Map collection _ -> layerReads collection
  Filter collection _ -> layerReads collection
  All collection _ -> layerReads collection
  None collection _ -> layerReads collection
  Some collection _ -> layerReads collection
  Reduce collection _ initial -> layerReads collection <> layerReads initial
  -- a custom operator is handed the whole datum
  CustomOp _ _ -> operands <> everything
  _ -> operands
  where
    operands = foldMap layerReads node

-- | A lookup of the path a node evaluates to: known only for a literal.
lookupOf :: Shape -> Reads
lookupOf (Literal key) = pathRead key
lookupOf _ = everything

-- | The path a literal names, as @var@ reads it: the data itself for
-- @null@ and @""@, otherwise the literal as a string.
pathRead :: Value -> Reads
pathRead key = case key of
  Null -> everything
  String s | Text.null s -> everything
  _ -> Reads (Set.singleton (toJsString (fromValue key))) False

-- | @missing@ looks up each key, and if its first operand is an array, that
-- array is the key list instead. A key that is itself an array is a @var@
-- argument list, which is not analysed.
missingLookups :: [Shape] -> Reads
missingLookups keys = case keys of
  ArrayOf listed : _ -> foldMap lookupOf listed
  _ -> foldMap lookupOf keys

-- | @missing_some@ looks up each of its options, or the one option if they
-- are not an array.
optionLookups :: Shape -> Reads
optionLookups (ArrayOf listed) = foldMap lookupOf listed
optionLookups other = lookupOf other

everything :: Reads
everything = Reads Set.empty True
