{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : JsonLogic.Core.Custom
--
-- Operators supplied by the caller: the one open extension point of the
-- rule language. A registry is built once, checked when it is built, and
-- handed to 'JsonLogic.compile', which resolves every custom operator in the
-- rule against it.
--
-- Two kinds of operator exist. An /eager/ operator ('pureOperator') is a
-- function of its already-evaluated arguments. A /lazy/ operator
-- ('lazyOperator') is handed its operands unevaluated and decides which to
-- evaluate, so it can short-circuit: a @coalesce@, a @try@, an @and_then@.
-- Both are pure. An operator that needs the clock or a database is given
-- that information in the data, by the caller, before the rule runs; this
-- keeps evaluation deterministic and its limits meaningful.
module JsonLogic.Core.Custom
  ( CustomOperator (..)
  , pureOperator
  , lazyOperator
  , Operand (..)
  , Step (..)
  , force
  , stepFail
  , operatorArity
  , Operators
  , noOperators
  , operatorsFromList
  , combineOperators
  , lookupOperator
  , operatorNames
  ) where

import Control.Monad (foldM)
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Text as Text

import JsonLogic.Core.Errors (Arity (..), CustomError, EvalError (..), OperatorsError (..))
import JsonLogic.Core.Operator (OperatorName (..), lookupBuiltin)
import JsonLogic.Core.Run (At, Run, failAt)
import JsonLogic.Core.Value (Val, toValue)

-- | An operand of a lazy operator, not yet evaluated. Evaluating it
-- ('force') spends steps and can fail like any rule node; evaluating it
-- twice is evaluating it twice.
newtype Operand = Operand (Run Val)

-- | What a lazy operator computes in: evaluating operands, and failing.
-- It runs inside the evaluator's step budget, so a lazy operator cannot
-- escape the limits by choosing what to evaluate.
newtype Step a = Step {runStep :: At -> OperatorName -> Run a}

instance Functor Step where
  fmap f (Step m) = Step $ \at name -> fmap f (m at name)

instance Applicative Step where
  pure a = Step $ \_ _ -> pure a
  Step mf <*> Step ma = Step $ \at name -> mf at name <*> ma at name

instance Monad Step where
  Step m >>= k = Step $ \at name -> m at name >>= \a -> runStep (k a) at name

-- | Evaluate an operand, in the data scope the operator was called in.
force :: Operand -> Step Aeson.Value
force (Operand run) = Step $ \_ _ -> fmap toValue run

-- | Stop the evaluation with 'JsonLogic.Core.Errors.CustomOperatorFailed'.
stepFail :: CustomError -> Step a
stepFail reason = Step $ \at name -> failAt at (CustomOperatorFailed name reason)

-- | A caller-supplied operator, with the number of arguments it takes.
--
-- The contract, in full:
--
-- * In strict arity mode the library checks the number of operands when
--   the rule is compiled, and refuses a rule that gives the wrong number.
--   In lenient mode the count is not checked, and the operator checks its
--   own.
-- * The data argument is the datum in scope: inside @map@, @filter@ and the
--   other iterating operators, the current element. It is converted to JSON
--   lazily, so an operator that ignores it pays nothing for it.
-- * Arguments and result are JSON. JavaScript values with no JSON form
--   cross the boundary as @null@: @NaN@, the infinities and @undefined@.
-- * An eager operator's operands are all evaluated, left to right, before
--   it runs; a lazy operator is handed them unevaluated.
-- * A failure ('Left', or 'stepFail') stops the evaluation with
--   'JsonLogic.Core.Errors.CustomOperatorFailed', carrying the 'CustomError'
--   unchanged.
-- * The result is held to the configured depth and step limits; the
--   operator's own work is not (the library cannot see inside it).
data CustomOperator
  = Eager !Arity (Aeson.Value -> [Aeson.Value] -> Either CustomError Aeson.Value)
  | Lazy !Arity (Aeson.Value -> [Operand] -> Step Aeson.Value)

-- | An eager operator: a pure function of the datum and the evaluated
-- operands.
pureOperator :: Arity -> (Aeson.Value -> [Aeson.Value] -> Either CustomError Aeson.Value) -> CustomOperator
pureOperator = Eager

-- | A lazy operator: it receives its operands unevaluated and 'force's the
-- ones it needs.
lazyOperator :: Arity -> (Aeson.Value -> [Operand] -> Step Aeson.Value) -> CustomOperator
lazyOperator = Lazy

-- | How many operands an operator takes.
operatorArity :: CustomOperator -> Arity
operatorArity (Eager arity _) = arity
operatorArity (Lazy arity _) = arity

-- | A registry of custom operators.
newtype Operators = Operators (Map.Map OperatorName CustomOperator)

-- | No custom operators: any operator outside the builtin set is unknown.
noOperators :: Operators
noOperators = Operators Map.empty

-- | Custom operators from an association list. A name that is a builtin
-- operator, that is empty or has leading or trailing whitespace, or that
-- appears twice, is refused rather than silently ignored.
operatorsFromList :: [(OperatorName, CustomOperator)] -> Either OperatorsError Operators
operatorsFromList entries = Operators <$> foldM add Map.empty entries
  where
    add table (name@(OperatorName text), implementation)
      | Text.null text || Text.strip text /= text = Left (InvalidOperatorName name)
      | isJust (lookupBuiltin text) = Left (ReservedOperatorName name)
      | Map.member name table = Left (DuplicateOperatorName name)
      | otherwise = Right (Map.insert name implementation table)

-- | Combine two registries. A name defined by both is an error: neither
-- definition silently wins.
combineOperators :: Operators -> Operators -> Either OperatorsError Operators
combineOperators (Operators left) (Operators right) =
  case Map.keys (Map.intersection left right) of
    [] -> Right (Operators (Map.union left right))
    name : _ -> Left (DuplicateOperatorName name)

-- | The operator registered under a name.
lookupOperator :: Operators -> OperatorName -> Maybe CustomOperator
lookupOperator (Operators table) name = Map.lookup name table

-- | The names in a registry, in order.
operatorNames :: Operators -> [OperatorName]
operatorNames (Operators table) = Map.keys table
