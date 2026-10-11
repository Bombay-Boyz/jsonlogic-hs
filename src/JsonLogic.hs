-- |
-- Module      : JsonLogic
--
-- The public entry point: apply a JsonLogic rule to some data.
--
-- > apply rule data
--
-- Both are aeson 'Value's. Custom operators, if any, go through
-- 'applyWith'.
module JsonLogic
  ( -- * Applying rules
    apply
  , applyWith

    -- * Custom operators
  , CustomOperator
  , Operators
  , noOperators
  , operatorsFromList
  , OperatorName (..)

    -- * Errors
  , Error (..)
  , ParseError (..)
  , EvalError (..)
  , Arity (..)
  ) where

import Data.Aeson (Value)
import Data.Bifunctor (first)

import JsonLogic.Core.Errors (Arity (..), Error (..), EvalError (..), ParseError (..))
import JsonLogic.Core.Eval
  ( CustomOperator
  , Operators
  , evaluateWith
  , noOperators
  , operatorsFromList
  )
import JsonLogic.Core.Parse (parseRule)
import JsonLogic.Core.Rule (OperatorName (..))

-- | Apply a rule to some data, with the builtin operators only.
apply :: Value -> Value -> Either Error Value
apply = applyWith noOperators

-- | Apply a rule to some data, with extra operators available.
applyWith :: Operators -> Value -> Value -> Either Error Value
applyWith operators rule input = do
  parsed <- first ParseFailure (parseRule rule)
  first EvalFailure (evaluateWith operators parsed input)
