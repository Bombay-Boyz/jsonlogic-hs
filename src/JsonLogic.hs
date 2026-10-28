{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : JsonLogic
--
-- Apply JsonLogic rules (<https://jsonlogic.com>) to data, with the
-- semantics of the JavaScript reference implementation, json-logic-js 2.0.5.
--
-- > apply rule data
--
-- Both are aeson 'Value's. The default 'Config' bounds nesting depth,
-- evaluation steps, string length and array length, so a rule from an
-- untrusted source cannot exhaust time or memory.
--
-- To run one rule against many inputs, 'compile' it once and 'run' it:
--
-- > case compile defaultConfig noOperators rule of
-- >   Left err -> ...
-- >   Right compiled -> map (run compiled) inputs
--
-- Every 'Error' carries the 'Location' in the rule where it happened, as a
-- JSON Pointer.
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
  , tryRun

    -- * What a rule reads
  , DataUse (..)
  , dataUse

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
  , errorLocation
  , renderError

    -- * Locations
  , Location
  , Segment (..)
  , locationSegments
  , fromSegments
  , renderLocation
  ) where

import Control.DeepSeq (NFData (..), force)
import Control.Exception (SomeException, evaluate, try)
import Data.Aeson (Value)
import Data.Bifunctor (first)

import JsonLogic.Core.Analysis (DataUse (..))
import qualified JsonLogic.Core.Analysis as Analysis
import JsonLogic.Core.Config (ArityMode (..), Config (..), defaultConfig)
import JsonLogic.Core.Custom (CustomOperator, Operators, noOperators, operatorsFromList)
import JsonLogic.Core.Errors
  ( Arity (..)
  , Error (..)
  , EvalError (..)
  , LimitKind (..)
  , OperatorsError (..)
  , ParseError (..)
  , errorLocation
  , renderError
  )
import JsonLogic.Core.Eval (Program, compileRule, runProgram)
import JsonLogic.Core.Location (Location, Segment (..), fromSegments, locationSegments, renderLocation)
import JsonLogic.Core.Operator (OperatorName (..))
import JsonLogic.Core.Parse (parseRuleWith)

-- | A rule that has been read, checked and compiled, ready to run against
-- any number of inputs. It keeps the 'Config' and the custom operators it was
-- compiled with, and nothing of the JSON it was read from.
data Rule = Rule !DataUse !Program

-- | A 'Rule' is compiled completely when it is built, so there is nothing
-- left to evaluate.
instance NFData Rule where
  rnf (Rule use program) = rnf use `seq` program `seq` ()

-- | Read a rule and compile it. Fails with a 'ParseFailure' if the rule
-- has too many nodes, nests too deeply or, in 'Strict' mode, gives an
-- operator the wrong number of arguments. Custom operators are resolved
-- here, once; an operator the registry does not have is reported as
-- 'UnknownOperator' only if a run reaches it, because the reference
-- evaluates rules lazily.
--
-- All of the work is done when the 'Right' is demanded, not on the first
-- 'run'.
compile :: Config -> Operators -> Value -> Either Error Rule
compile config operators value = do
  tree <- first (uncurry ParseFailure) (parseRuleWith config value)
  let !use = force (Analysis.dataUse tree)
      !program = compileRule config operators tree
  pure (Rule use program)

-- | Run a compiled rule against some data.
run :: Rule -> Value -> Either Error Value
run (Rule _ program) input = first (uncurry EvalFailure) (runProgram program input)

-- | 'run', for callers who cannot let an exception escape: a custom
-- operator is ordinary Haskell and may throw (or loop), and 'run' being pure
-- does not stop it. The result is forced inside the 'try', so an exception
-- raised while a lazy part of it is evaluated is caught too. Wrap it in
-- 'System.Timeout.timeout' to bound wall-clock time as well.
tryRun :: Rule -> Value -> IO (Either SomeException (Either Error Value))
tryRun rule input = try (evaluate (force (run rule input)))

-- | What a compiled rule can read from its data, found without running it:
-- if 'usesAllData' is false, its result depends on the data only through
-- the values at 'usedPaths'. Unlike the reference's @uses_data@, a @var@
-- inside a @map@ (or other iteration) body is not counted, because it reads
-- the element, not the data. See "JsonLogic.Core.Analysis".
--
-- > dataUse <$> compile defaultConfig noOperators rule
dataUse :: Rule -> DataUse
dataUse (Rule use _) = use

-- | Apply a rule to some data, with the builtin operators only and the
-- default 'Config'.
apply :: Value -> Value -> Either Error Value
apply = applyWith noOperators

-- | Apply a rule to some data, with extra operators available and the
-- default 'Config'.
applyWith :: Operators -> Value -> Value -> Either Error Value
applyWith = applyWithConfig defaultConfig

-- | Apply a rule to some data with an explicit 'Config' and extra
-- operators. Equal to 'compile' then 'run'.
applyWithConfig :: Config -> Operators -> Value -> Value -> Either Error Value
applyWithConfig config operators rule input = do
  compiled <- compile config operators rule
  run compiled input
