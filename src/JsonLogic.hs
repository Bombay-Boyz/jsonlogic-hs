-- |
-- Module      : JsonLogic
--
-- The public entry point: apply a JsonLogic rule to some data.
--
-- > apply rule data
--
-- Both are aeson 'Value's. For rules from an untrusted source, the default
-- @Config@ already bounds nesting depth, evaluation steps, string length
-- and array length; see @Config@.
--
-- To run one rule many times, 'compile' it once and 'run' it:
--
-- > case compile defaultConfig rule of
-- >   Left err -> ...
-- >   Right compiled -> map (run noOperators compiled) inputs
--
-- This module is the whole supported API. The @JsonLogic.Core.*@ modules
-- are exposed for the test suites and for advanced use; they carry no
-- stability promise before 1.0 (see docs/STABILITY.md).
module JsonLogic
  ( -- * Applying rules
    apply
  , applyWith
  , applyWithConfig

    -- * Compiling a rule once
  , Rule
  , compile
  , run

    -- * Configuration
  , Config (..)
  , ArityMode (..)
  , defaultConfig

    -- * Custom operators
  , CustomOperator
  , Operators
  , noOperators
  , operatorsFromList
  , OperatorsError (..)
  , OperatorName (..)

    -- * Errors
  , Error (..)
  , ParseError (..)
  , EvalError (..)
  , LimitKind (..)
  , Arity (..)
  , renderError
  ) where

import Data.Aeson (Value)
import Data.Bifunctor (first)

import JsonLogic.Core.Config (ArityMode (..), Config (..), defaultConfig)
import JsonLogic.Core.Errors
  ( Arity (..)
  , Error (..)
  , EvalError (..)
  , LimitKind (..)
  , ParseError (..)
  , renderError
  )
import JsonLogic.Core.Eval
  ( CustomOperator
  , Operators
  , OperatorsError (..)
  , evaluateWith
  , noOperators
  , operatorsFromList
  )
import JsonLogic.Core.Parse (parseRuleWith)
import JsonLogic.Core.Rule (OperatorName (..))
import qualified JsonLogic.Core.Rule as Core

-- | A rule read and checked once, ready to be run against many inputs. It
-- remembers the @Config@ it was compiled with, so its limits travel with it.
data Rule = Rule Config Core.Rule

-- | Read a rule. Fails with a 'ParseFailure' if it nests too deeply, or (in
-- 'Strict' mode) gives an operator the wrong number of arguments.
compile :: Config -> Value -> Either Error Rule
compile config value = Rule config <$> first ParseFailure (parseRuleWith config value)

-- | Run a compiled rule against some data.
run :: Operators -> Rule -> Value -> Either Error Value
run operators (Rule config rule) input =
  first EvalFailure (evaluateWith config operators rule input)

-- | Apply a rule to some data, with the builtin operators only and the
-- default @Config@.
apply :: Value -> Value -> Either Error Value
apply = applyWith noOperators

-- | Apply a rule to some data, with extra operators available and the
-- default @Config@.
applyWith :: Operators -> Value -> Value -> Either Error Value
applyWith = applyWithConfig defaultConfig

-- | Apply a rule to some data with an explicit @Config@ and extra
-- operators. Equal to 'compile' then 'run'.
applyWithConfig :: Config -> Operators -> Value -> Value -> Either Error Value
applyWithConfig config operators rule input = do
  compiled <- compile config rule
  run operators compiled input
