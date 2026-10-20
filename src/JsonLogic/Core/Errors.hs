{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : JsonLogic.Core.Errors
--
-- The closed error vocabulary of the pipeline, layered by stage
-- (VISION.md §7): reading a rule can fail, evaluating one can fail, and
-- 'Error' is the sum of the two for callers that do both.
--
-- Every constructor is stable public API; callers should match on the
-- constructors, never on rendered text. 'renderError' produces a message
-- for humans and logs, and its wording may change between releases.
module JsonLogic.Core.Errors
  ( Arity (..)
  , Error (..)
  , EvalError (..)
  , LimitKind (..)
  , OperatorsError (..)
  , ParseError (..)
  , renderError
  , renderEvalError
  , renderParseError
  ) where

import Data.Text (Text)
import qualified Data.Text as Text

import JsonLogic.Core.Rule (OperatorName (..))

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
    -- admit: operator, what it accepts, how many it was given. Only raised
    -- in strict arity mode, and for @*@ with no operands, which the
    -- reference also rejects.
    ArityMismatch OperatorName Arity Int
  deriving (Eq, Show)

-- | Which resource limit stopped an evaluation, with the limit that was
-- configured. The limits bound work and memory, not wall-clock time.
data LimitKind
  = -- | More evaluation steps than allowed. A step is one rule node
    -- evaluated, one iteration of @map@, @filter@, @reduce@, @all@, @some@
    -- or @none@, or one element or character produced by @cat@ or @merge@.
    StepLimit Int
  | -- | A string result longer than allowed (in UTF-16 code units).
    StringLimit Int
  | -- | An array result longer than allowed.
    ArrayLimit Int
  | -- | A value, such as a custom operator's result, nested deeper than
    -- allowed.
    ValueDepthLimit Int
  deriving (Eq, Show)

-- | Why a well-formed rule could not be evaluated. JavaScript semantics
-- make most odd inputs produce @NaN@ or @null@ rather than fail, so only
-- operators the evaluator was not given, the few operands the reference
-- itself throws on, and exhausted limits can go wrong.
data EvalError
  = -- | A rule used an operator that is neither builtin nor supplied.
    UnknownOperator OperatorName
  | -- | A supplied operator reported failure, with its reason.
    CustomOperatorFailed OperatorName Text
  | -- | An operand the reference implementation throws a @TypeError@ on
    -- (for example @{"missing_some": [1, null]}@), with the operator.
    InvalidOperand OperatorName
  | -- | A configured resource limit was exceeded.
    LimitExceeded LimitKind
  deriving (Eq, Show)

-- | Why a registry of custom operators was refused. Raised when the
-- registry is built, before any rule is read or evaluated.
data OperatorsError
  = -- | The name belongs to a builtin operator, which would shadow it.
    ReservedOperatorName OperatorName
  | -- | The name was given more than once.
    DuplicateOperatorName OperatorName
  deriving (Eq, Show)

-- | Either stage's failure, for callers that parse and evaluate in one go.
data Error
  = ParseFailure ParseError
  | EvalFailure EvalError
  deriving (Eq, Show)

-- | A one-line description of an error, for logs and messages.
renderError :: Error -> Text
renderError (ParseFailure e) = renderParseError e
renderError (EvalFailure e) = renderEvalError e

-- | A one-line description of a parse error.
renderParseError :: ParseError -> Text
renderParseError (DepthExceeded limit) =
  "rule nests deeper than the limit of " <> shown limit
renderParseError (ArityMismatch (OperatorName name) accepted given) =
  "operator " <> quoted name <> " takes " <> renderArity accepted <> ", but was given " <> shown given

-- | A one-line description of an evaluation error.
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
  ArrayLimit n -> "an array result exceeded the limit of " <> shown n <> " elements"
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
