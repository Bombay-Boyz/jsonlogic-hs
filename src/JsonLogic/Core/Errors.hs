{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : JsonLogic.Core.Errors
--
-- The closed error vocabulary, layered by stage: reading a rule can fail
-- ('ParseError'), evaluating one can fail ('EvalError'), and 'Error' is
-- either, together with the 'Location' in the rule where it happened.
--
-- Callers match on the constructors, never on rendered text. 'renderError'
-- produces a message for people and logs; its wording may change in any
-- release.
module JsonLogic.Core.Errors
  ( Arity (..)
  , Error (..)
  , EvalError (..)
  , LimitKind (..)
  , OperatorsError (..)
  , ParseError (..)
  , errorLocation
  , renderError
  , renderEvalError
  , renderParseError
  ) where

import Control.DeepSeq (NFData (..))
import Control.Exception (Exception)
import Data.Text (Text)
import qualified Data.Text as Text

import JsonLogic.Core.Location (Location, renderLocation)
import JsonLogic.Core.Operator (OperatorName (..))

-- | How many arguments an operator accepts.
data Arity
  = Exactly Int
  | AtLeast Int
  | Between Int Int
  deriving (Eq, Show)

-- | Why a JSON value could not be read as a rule.
data ParseError
  = -- | The rule, or a literal inside it, nests deeper than the configured
    -- limit (carried here). Raised before evaluation ever sees the rule.
    DepthExceeded Int
  | -- | An operator was given a number of arguments it does not take:
    -- the operator, what it accepts, and how many it was given. Raised in
    -- strict arity mode only.
    ArityMismatch OperatorName Arity Int
  | -- | The rule's JSON has more nodes than the configured limit (carried
    -- here). Raised before anything is built.
    NodeLimitExceeded Int
  deriving (Eq, Show)

-- | Which resource limit stopped an evaluation, with the configured
-- limit. The limits bound work and memory, not wall-clock time.
data LimitKind
  = -- | More evaluation steps than allowed (see @maxSteps@ in "JsonLogic.Core.Config"
    -- for what a step is).
    StepLimit Int
  | -- | A string result longer than allowed, in UTF-16 code units.
    StringLimit Int
  | -- | An array result, or an array to iterate, longer than allowed.
    ArrayLimit Int
  | -- | A value a custom operator returned nests deeper than allowed.
    ValueDepthLimit Int
  deriving (Eq, Show)

-- | Why a well-formed rule could not be evaluated. JavaScript semantics
-- turn most odd inputs into @NaN@ or @null@ rather than a failure, so only
-- an operator nobody supplied, the few operands the reference itself throws
-- on, a custom operator's own failure, and an exhausted limit can go wrong.
data EvalError
  = -- | The rule reached an operator that is neither builtin nor supplied.
    UnknownOperator OperatorName
  | -- | A supplied operator reported failure, with its reason.
    CustomOperatorFailed OperatorName Text
  | -- | An operand the reference implementation throws a @TypeError@ on:
    -- @missing_some@ with a @null@ or missing option list, and @*@ with no
    -- operands at all (reached in lenient arity mode only; strict mode
    -- rejects it as an 'ArityMismatch' before evaluating).
    InvalidOperand OperatorName
  | -- | A configured resource limit was exceeded.
    LimitExceeded LimitKind
  deriving (Eq, Show)

-- | Why a registry of custom operators was refused. Raised when the
-- registry is built, before any rule is read.
data OperatorsError
  = -- | The name belongs to a builtin operator, which would shadow it.
    ReservedOperatorName OperatorName
  | -- | The name was given more than once.
    DuplicateOperatorName OperatorName
  deriving (Eq, Show)

-- | A failure of either stage, and where in the rule it happened.
data Error
  = -- | The rule could not be read. The location is the node that could not
    -- be read: the operator with the wrong argument count, or the first
    -- value that nests too deeply.
    ParseFailure Location ParseError
  | -- | The rule could not be evaluated. The location is the node being
    -- evaluated when it failed: the unknown or failing operator, the
    -- operator given an invalid operand, or the node that ran out of a
    -- resource.
    EvalFailure Location EvalError
  deriving (Eq, Show)

-- | Where an error happened.
errorLocation :: Error -> Location
errorLocation (ParseFailure location _) = location
errorLocation (EvalFailure location _) = location

-- | A one-line description of an error and its location, for logs and
-- messages.
--
-- > at /if/1: operator "*" takes at least 1 argument, but was given 0
renderError :: Error -> Text
renderError e = case e of
  ParseFailure location problem -> at location (renderParseError problem)
  EvalFailure location problem -> at location (renderEvalError problem)
  where
    at location message = case renderLocation location of
      "" -> "at the root of the rule: " <> message
      pointer -> "at " <> pointer <> ": " <> message

-- | A one-line description of a parse error, without its location.
renderParseError :: ParseError -> Text
renderParseError (DepthExceeded limit) =
  "the rule nests deeper than the limit of " <> shown limit
renderParseError (ArityMismatch (OperatorName name) accepted given) =
  "operator " <> quoted name <> " takes " <> renderArity accepted <> ", but was given " <> shown given
renderParseError (NodeLimitExceeded limit) =
  "the rule has more than the limit of " <> shown limit <> " nodes"

-- | A one-line description of an evaluation error, without its location.
renderEvalError :: EvalError -> Text
renderEvalError (UnknownOperator (OperatorName name)) =
  "unknown operator " <> quoted name
renderEvalError (CustomOperatorFailed (OperatorName name) reason) =
  "custom operator " <> quoted name <> " failed: " <> reason
renderEvalError (InvalidOperand (OperatorName name)) =
  "operator " <> quoted name <> " was given an operand it cannot work with"
renderEvalError (LimitExceeded kind) = case kind of
  StepLimit n -> "evaluation exceeded the limit of " <> shown n <> " steps"
  StringLimit n -> "a string result exceeded the limit of " <> shown n <> " code units"
  ArrayLimit n -> "an array exceeded the limit of " <> shown n <> " elements"
  ValueDepthLimit n -> "a value nests deeper than the limit of " <> shown n

renderArity :: Arity -> Text
renderArity (Exactly n) = "exactly " <> shown n <> plural n
renderArity (AtLeast n) = "at least " <> shown n <> plural n
renderArity (Between low high) = "between " <> shown low <> " and " <> shown high <> " arguments"

plural :: Int -> Text
plural 1 = " argument"
plural _ = " arguments"

shown :: Int -> Text
shown = Text.pack . show

quoted :: Text -> Text
quoted name = "\"" <> name <> "\""

-- | So that an 'Error' can be thrown, for callers that prefer exceptions.
instance Exception Error

instance NFData Arity where
  rnf (Exactly n) = rnf n
  rnf (AtLeast n) = rnf n
  rnf (Between low high) = rnf low `seq` rnf high

instance NFData ParseError where
  rnf (DepthExceeded n) = rnf n
  rnf (ArityMismatch name arity given) = rnf name `seq` rnf arity `seq` rnf given
  rnf (NodeLimitExceeded n) = rnf n

instance NFData LimitKind where
  rnf (StepLimit n) = rnf n
  rnf (StringLimit n) = rnf n
  rnf (ArrayLimit n) = rnf n
  rnf (ValueDepthLimit n) = rnf n

instance NFData EvalError where
  rnf (UnknownOperator name) = rnf name
  rnf (CustomOperatorFailed name reason) = rnf name `seq` rnf reason
  rnf (InvalidOperand name) = rnf name
  rnf (LimitExceeded kind) = rnf kind

instance NFData OperatorsError where
  rnf (ReservedOperatorName name) = rnf name
  rnf (DuplicateOperatorName name) = rnf name

instance NFData Error where
  rnf (ParseFailure location problem) = rnf location `seq` rnf problem
  rnf (EvalFailure location problem) = rnf location `seq` rnf problem
