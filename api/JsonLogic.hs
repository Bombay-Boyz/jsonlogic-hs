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
-- untrusted source cannot exhaust time or memory, and refuses a rule that
-- names an operator nobody supplied.
--
-- To run one rule against many inputs, 'compile' it once and 'run' it:
--
-- > case compile defaultConfig noOperators rule of
-- >   Left err -> ...
-- >   Right compiled -> map (run compiled) inputs
--
-- Every 'Error' carries the 'Location' in the rule where it happened, as a
-- JSON Pointer, and has a 'ToJSON' instance for responses to programs.
-- 'validate' reports every problem in a rule, not only the first.
--
-- This module is the whole supported API. The @JsonLogic.Core.*@ modules
-- are exposed for the test suites and for advanced use; they carry no
-- stability promise before 1.0 (see docs/STABILITY.md).
module JsonLogic
  ( -- * Applying rules
    apply
  , applyWith
  , applyWithConfig
  , applyBytes
  , applyBytesWith
  , BytesError (..)

    -- * Compiling a rule once
  , Rule
  , compile
  , run
  , tryRun
  , runWithTimeout
  , RunFailure (..)

    -- * Checking before running
  , validate
  , checkData
  , unresolvedOperators

    -- * What a rule reads
  , DataUse (..)
  , dataUse

    -- * Configuration
  , Config
      ( maxDepth
      , maxSteps
      , maxStringLength
      , maxArrayLength
      , maxIterations
      , maxNodes
      , maxDataDepth
      , arityMode
      , unknownOperators
      , numericSafety
      )
  , ArityMode (..)
  , UnknownOperators (..)
  , NumericSafety (..)
  , ConfigError (..)
  , defaultConfig
  , unlimited
  , validateConfig

    -- * Custom operators
  , CustomOperator
  , pureOperator
  , lazyOperator
  , Operand
  , Step
  , force
  , stepFail
  , CustomError (..)
  , customFailure
  , Operators
  , noOperators
  , operatorsFromList
  , combineOperators
  , OperatorsError (..)
  , OperatorName (..)

    -- * Errors
  , Error (..)
  , ParseError (..)
  , EvalError (..)
  , OperandIssue (..)
  , LimitKind (..)
  , Arity (..)
  , errorLocation
  , errorKind
  , renderError

    -- * Locations
  , Location
  , Segment (..)
  , locationSegments
  , fromSegments
  , renderLocation
  ) where

import Control.DeepSeq (NFData (..))
import qualified Control.DeepSeq as DeepSeq
import Control.Exception (SomeAsyncException (..), SomeException, evaluate, fromException, throwIO, try)
import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as LazyByteString
import System.Timeout (timeout)

import JsonLogic.Core.Analysis (DataUse (..))
import qualified JsonLogic.Core.Analysis as Analysis
import JsonLogic.Core.Check (unknownOperatorsIn)
import JsonLogic.Core.Config
  ( ArityMode (..)
  , Config (..)
  , ConfigError (..)
  , NumericSafety (..)
  , UnknownOperators (..)
  , defaultConfig
  , unlimited
  , validateConfig
  )
import JsonLogic.Core.Custom
  ( CustomOperator
  , Operand
  , Operators
  , Step
  , combineOperators
  , force
  , lazyOperator
  , noOperators
  , operatorsFromList
  , pureOperator
  , stepFail
  )
import JsonLogic.Core.Errors
  ( Arity (..)
  , CustomError (..)
  , Error (..)
  , EvalError (..)
  , LimitKind (..)
  , OperandIssue (..)
  , OperatorsError (..)
  , ParseError (..)
  , customFailure
  , errorKind
  , errorLocation
  , renderError
  )
import JsonLogic.Core.Eval (Program, compileRule, runProgram)
import JsonLogic.Core.Location (Location, Segment (..), fromSegments, locationSegments, renderLocation, root)
import JsonLogic.Core.Operator (OperatorName (..))
import JsonLogic.Core.Parse (parseProblems, parseRuleChecked)
import JsonLogic.Core.Value (firstLossyNumber, nestsDeeperThan)

-- | A rule that has been read, checked and compiled, ready to run against
-- any number of inputs. It keeps the 'Config' and the custom operators it was
-- compiled with, and nothing of the JSON it was read from.
data Rule = Rule !DataUse ![(Location, OperatorName)] !Program

-- | A 'Rule' is compiled completely when it is built, so there is nothing
-- left to evaluate.
instance NFData Rule where
  rnf (Rule use unknown program) = rnf use `seq` rnf unknown `seq` program `seq` ()

-- | Read a rule and compile it. Fails with a 'ParseFailure' if the
-- configuration is invalid, the rule has too many nodes or nests too deeply,
-- and, with the default 'Config', if it gives an operator the wrong number of
-- arguments or names an operator that is neither builtin nor registered (the
-- error names the nearest known operators). 'Lenient' arity mode, or
-- 'FailWhenReached', accepts such a rule as the reference does and fails
-- only if a run reaches the operator; 'unresolvedOperators' lists them.
--
-- All of the work is done when the 'Right' is demanded, not on the first
-- 'run'.
compile :: Config -> Operators -> Value -> Either Error Rule
compile config operators value = do
  tree <- first (uncurry ParseFailure) (parseRuleChecked config (Just operators) value)
  let !use = DeepSeq.force (Analysis.dataUse tree)
      !unknown = DeepSeq.force (unknownOperatorsIn operators tree)
      !program = compileRule config operators tree
  pure (Rule use unknown program)

-- | Run a compiled rule against some data.
run :: Rule -> Value -> Either Error Value
run (Rule _ _ program) input = first (uncurry EvalFailure) (runProgram program input)

-- | Why 'tryRun' did not produce an answer.
data RunFailure
  = -- | The rule or a limit said no: a value, not a bug.
    Rejected Error
  | -- | A custom operator threw an exception (or one was raised while its
    -- result was forced). Asynchronous exceptions are never reported here.
    OperatorThrew SomeException
  | -- | Only from 'runWithTimeout': the time ran out.
    TimedOut
  deriving (Show)

-- | 'run', for callers who cannot let an exception escape: a custom
-- operator is ordinary Haskell and may throw, and 'run' being pure does not
-- stop it. The result is forced inside the 'try', so an exception raised
-- while a lazy part of it is evaluated is caught too.
--
-- Only /synchronous/ exceptions are caught. An asynchronous one (a
-- 'System.Timeout.timeout', a @killThread@, a stack or heap overflow, an
-- interrupt) is the caller's to receive, so it is re-thrown: swallowing it
-- would leave a cancelled thread running and make every enclosing timeout
-- report success.
tryRun :: Rule -> Value -> IO (Either RunFailure Value)
tryRun rule input = do
  outcome <- try (evaluate (DeepSeq.force (run rule input)))
  case outcome of
    Right (Right result) -> pure (Right result)
    Right (Left problem) -> pure (Left (Rejected problem))
    Left exception
      | Just (SomeAsyncException _) <- fromException exception -> throwIO exception
      | otherwise -> pure (Left (OperatorThrew exception))

-- | 'tryRun' under a wall-clock limit, in microseconds. The limits in
-- 'Config' bound work and memory, not time: a custom operator that is slow is
-- still slow, and this is how to bound it.
runWithTimeout :: Int -> Rule -> Value -> IO (Either RunFailure Value)
runWithTimeout microseconds rule input =
  maybe (Left TimedOut) id <$> timeout microseconds (tryRun rule input)

-- | Every problem 'compile' would find in a rule, not only the first, in
-- document order, so that a rule can be fixed in one pass. Empty exactly when
-- 'compile' succeeds (and the first element is the error 'compile' returns).
validate :: Config -> Operators -> Value -> [Error]
validate config operators value =
  map (uncurry ParseFailure) (parseProblems config (Just operators) value)

-- | Screen untrusted data once, before running rules against it: it must not
-- nest deeper than 'maxDataDepth' and, under 'RejectLossy', must hold no
-- number a double cannot carry. Evaluation itself does not walk the data.
checkData :: Config -> Value -> Either Error ()
checkData config input
  | nestsDeeperThan (maxDataDepth config) input =
      Left (EvalFailure root (LimitExceeded (DataDepthLimit (maxDataDepth config))))
  | numericSafety config == RejectLossy
  , Just n <- firstLossyNumber input =
      Left (EvalFailure root (LossyNumber n))
  | otherwise = Right ()

-- | The operators in a compiled rule that are neither builtin nor
-- registered, and where each is. With the default configuration there are
-- none (the rule would not have compiled); under 'FailWhenReached' or
-- 'Lenient' they are the places a run can fail with 'UnknownOperator'.
unresolvedOperators :: Rule -> [(Location, OperatorName)]
unresolvedOperators (Rule _ unknown _) = unknown

-- | What a compiled rule can read from its data, found without running it:
-- if 'usesAllData' is false, its result depends on the data only through
-- the values at 'usedPaths'. Unlike the reference's @uses_data@, a @var@
-- inside a @map@ (or other iteration) body is not counted, because it reads
-- the element, not the data. See "JsonLogic.Core.Analysis".
--
-- > dataUse <$> compile defaultConfig noOperators rule
dataUse :: Rule -> DataUse
dataUse (Rule use _ _) = use

-- | Apply a rule to some data, with the builtin operators only and the
-- default 'Config'.
apply :: Value -> Value -> Either Error Value
apply = applyWith noOperators

-- | Apply a rule to some data, with extra operators available and the
-- default 'Config'.
applyWith :: Operators -> Value -> Value -> Either Error Value
applyWith = applyWithConfig defaultConfig

-- | Apply a rule to some data with an explicit 'Config' and extra
-- operators. Equal to 'compile' then 'run', without the data-use analysis
-- that only a kept 'Rule' needs.
applyWithConfig :: Config -> Operators -> Value -> Value -> Either Error Value
applyWithConfig config operators rule input = do
  tree <- first (uncurry ParseFailure) (parseRuleChecked config (Just operators) rule)
  first (uncurry EvalFailure) (runProgram (compileRule config operators tree) input)

-- | Why 'applyBytes' failed.
data BytesError
  = -- | The rule is not JSON.
    MalformedRule String
  | -- | The data is not JSON.
    MalformedData String
  | -- | The rule or the evaluation was refused.
    Refused Error
  deriving (Show)

-- | 'apply' on JSON text: the rule, the data, and the result.
applyBytes :: LazyByteString.ByteString -> LazyByteString.ByteString -> Either BytesError LazyByteString.ByteString
applyBytes = applyBytesWith defaultConfig noOperators

-- | 'applyBytes' with an explicit 'Config' and extra operators.
applyBytesWith
  :: Config -> Operators -> LazyByteString.ByteString -> LazyByteString.ByteString -> Either BytesError LazyByteString.ByteString
applyBytesWith config operators rule input = do
  ruleValue <- first MalformedRule (Aeson.eitherDecode rule)
  inputValue <- first MalformedData (Aeson.eitherDecode input)
  result <- first Refused (applyWithConfig config operators ruleValue inputValue)
  pure (Aeson.encode result)
