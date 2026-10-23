-- |
-- Module      : JsonLogic.Core.Custom
--
-- Operators supplied by the caller: the one open extension point of the
-- rule language. A registry is built once, checked when it is built, and
-- handed to 'JsonLogic.compile', which resolves every custom operator in the
-- rule against it.
module JsonLogic.Core.Custom
  ( CustomOperator
  , Operators
  , noOperators
  , operatorsFromList
  , lookupOperator
  ) where

import Control.Monad (foldM)
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Text (Text)

import JsonLogic.Core.Errors (OperatorsError (..))
import JsonLogic.Core.Operator (OperatorName (..), lookupBuiltin)

-- | A caller-supplied operator: given the data being evaluated against and
-- its already-evaluated arguments, produce a result or a reason it failed.
--
-- The contract, in full:
--
-- * All of the operator's operands are evaluated, left to right, before it
--   runs, as for the reference's own ordinary operators. An operator that
--   nobody supplied is therefore reported only after its operands have run,
--   and only if the rule reaches it.
-- * The data argument is the datum in scope: inside @map@, @filter@ and the
--   other iterating operators, the current element. It is converted to JSON
--   lazily, so an operator that ignores it pays nothing for it.
-- * Arguments and result are JSON. JavaScript values with no JSON form
--   cross the boundary as @null@: @NaN@, the infinities and @undefined@.
-- * The number of arguments is not checked by the library; the operator
--   checks its own.
-- * @Left reason@ stops the evaluation with
--   'JsonLogic.Core.Errors.CustomOperatorFailed'.
-- * The result is held to the configured depth and step limits; the
--   operator's own work is not (the library cannot see inside it).
type CustomOperator = Aeson.Value -> [Aeson.Value] -> Either Text Aeson.Value

-- | A registry of custom operators.
newtype Operators = Operators (Map.Map OperatorName CustomOperator)

-- | Registries combine left-biased: where both define a name, the left one
-- wins. Neither can hold a builtin name, so neither can the combination.
instance Semigroup Operators where
  Operators left <> Operators right = Operators (Map.union left right)

instance Monoid Operators where
  mempty = noOperators

-- | No custom operators: any operator outside the builtin set is an
-- 'JsonLogic.Core.Errors.UnknownOperator' when it is reached.
noOperators :: Operators
noOperators = Operators Map.empty

-- | Custom operators from an association list. A name that is a builtin
-- operator, or that appears twice, is refused rather than silently ignored.
operatorsFromList :: [(OperatorName, CustomOperator)] -> Either OperatorsError Operators
operatorsFromList entries = Operators <$> foldM add Map.empty entries
  where
    add table (name@(OperatorName text), implementation)
      | isJust (lookupBuiltin text) = Left (ReservedOperatorName name)
      | Map.member name table = Left (DuplicateOperatorName name)
      | otherwise = Right (Map.insert name implementation table)

-- | The operator registered under a name.
lookupOperator :: Operators -> OperatorName -> Maybe CustomOperator
lookupOperator (Operators table) name = Map.lookup name table
