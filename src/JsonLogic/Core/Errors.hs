-- |
-- Module      : JsonLogic.Core.Errors
--
-- The closed error vocabulary of the pipeline, layered by stage
-- (VISION.md §7): reading a rule can fail, evaluating one can fail, and
-- 'Error' is the sum of the two for callers that do both.
module JsonLogic.Core.Errors
  ( Arity (..)
  , Error (..)
  , EvalError (..)
  , ParseError (..)
  ) where

import Data.Text (Text)

import JsonLogic.Core.Rule (OperatorName)

-- | How many arguments an operator accepts.
data Arity
  = Exactly Int
  | AtLeast Int
  | Between Int Int
  deriving (Eq, Show)

-- | Why a JSON value could not be read as a rule.
data ParseError
  = -- | The rule nests deeper than the configured limit (carried here).
    -- Raised before evaluation ever sees the rule (Standard 2.4, 4.10).
    DepthExceeded Int
  | -- | An operator was given a number of arguments its shape does not
    -- admit: operator, what it accepts, how many it was given.
    ArityMismatch OperatorName Arity Int
  deriving (Eq, Show)

-- | Why a well-formed rule could not be evaluated. JavaScript semantics
-- make most odd inputs produce @NaN@ or @null@ rather than fail, so only
-- operators the evaluator was not given can go wrong.
data EvalError
  = -- | A rule used an operator that is neither builtin nor supplied.
    UnknownOperator OperatorName
  | -- | A supplied operator reported failure, with its reason.
    CustomOperatorFailed OperatorName Text
  deriving (Eq, Show)

-- | Either stage's failure, for callers that parse and evaluate in one go.
data Error
  = ParseFailure ParseError
  | EvalFailure EvalError
  deriving (Eq, Show)
