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
-- release. For programs, 'Error' has a 'ToJSON' instance with stable
-- @kind@ tags.
module JsonLogic.Core.Errors
  ( Arity (..)
  , CustomError (..)
  , customFailure
  , Error (..)
  , EvalError (..)
  , LimitKind (..)
  , OperandIssue (..)
  , OperatorsError (..)
  , ParseError (..)
  , errorKind
  , errorLocation
  , renderError
  , renderEvalError
  , renderParseError
  ) where

import Control.DeepSeq (NFData (..))
import Control.Exception (Exception (..))
import Data.Aeson (ToJSON (..), Value, object, (.=))
import Data.Scientific (FPFormat (..), Scientific, base10Exponent, formatScientific)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text

import JsonLogic.Core.Config (ConfigError (..))
import JsonLogic.Core.Location (Location, renderLocation)
import JsonLogic.Core.Operator (OperatorName (..))

-- | How many arguments an operator accepts.
data Arity
  = Exactly Int
  | AtLeast Int
  | Between Int Int
  deriving (Eq, Show)

-- | Why a custom operator failed, in a form a program can act on: a stable
-- code to match on, a message for people, and optional structured details.
-- With @OverloadedStrings@ a string literal is a failure with the code
-- @\"failed\"@, so @Left \"expected one number\"@ works.
data CustomError = CustomError
  { errorCode :: !Text
  , errorMessage :: !Text
  , errorDetails :: !(Maybe Value)
  }
  deriving (Eq, Show)

instance IsString CustomError where
  fromString = customFailure . Text.pack

-- | A failure with the code @\"failed\"@ and a message.
customFailure :: Text -> CustomError
customFailure message = CustomError "failed" message Nothing

-- | Which operand an operator could not work with, what it needed and what
-- it got.
data OperandIssue = OperandIssue
  { operandIndex :: !Int
  -- ^ Zero-based position among the operands.
  , operandExpected :: !Text
  , operandActual :: !Text
  }
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
  | -- | The rule names an operator that is neither builtin nor registered:
    -- the name, and the nearest known names (possibly none). Raised when
    -- the configuration refuses unknown operators at compile time.
    UnknownOperatorName OperatorName [OperatorName]
  | -- | The 'JsonLogic.Config' was refused.
    InvalidConfig ConfigError
  | -- | A number literal in the rule is not exactly a double (see
    -- 'JsonLogic.RejectLossy').
    LossyNumberLiteral Scientific
  deriving (Eq, Show)

-- | Which resource limit stopped an evaluation, with the configured
-- limit. The limits bound work and memory, not wall-clock time.
data LimitKind
  = -- | More evaluation steps than allowed (see @maxSteps@ in "JsonLogic.Core.Config"
    -- for what a step is).
    StepLimit Int
  | -- | A string result longer than allowed, in UTF-16 code units.
    StringLimit Int
  | -- | An array result longer than allowed.
    ArrayLimit Int
  | -- | An array to iterate longer than allowed.
    IterationLimit Int
  | -- | A value a custom operator returned nests deeper than allowed.
    ValueDepthLimit Int
  | -- | The data nests deeper than allowed (reported by 'JsonLogic.checkData').
    DataDepthLimit Int
  deriving (Eq, Show)

-- | Why a well-formed rule could not be evaluated. JavaScript semantics
-- turn most odd inputs into @NaN@ or @null@ rather than a failure, so only
-- an operator nobody supplied, the few operands the reference itself throws
-- on, a custom operator's own failure, and an exhausted limit can go wrong.
data EvalError
  = -- | The rule reached an operator that is neither builtin nor supplied.
    UnknownOperator OperatorName
  | -- | A supplied operator reported failure.
    CustomOperatorFailed OperatorName CustomError
  | -- | An operand the reference implementation throws a @TypeError@ on:
    -- @missing_some@ with a @null@ or missing option list, and @*@ with no
    -- operands at all (reached in lenient arity mode only; strict mode
    -- rejects it as an 'ArityMismatch' before evaluating).
    InvalidOperand OperatorName OperandIssue
  | -- | A configured resource limit was exceeded.
    LimitExceeded LimitKind
  | -- | A number that is not exactly a double reached evaluation under
    -- 'JsonLogic.RejectLossy'.
    LossyNumber Scientific
  deriving (Eq, Show)

-- | Why a registry of custom operators was refused. Raised when the
-- registry is built, before any rule is read.
data OperatorsError
  = -- | The name belongs to a builtin operator, which would shadow it.
    ReservedOperatorName OperatorName
  | -- | The name was given more than once.
    DuplicateOperatorName OperatorName
  | -- | The name is empty, or has leading or trailing whitespace, so no
    -- rule key could match it as intended.
    InvalidOperatorName OperatorName
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
renderParseError (UnknownOperatorName (OperatorName name) near) =
  "unknown operator " <> quoted name <> didYouMean near
renderParseError (InvalidConfig (NegativeLimit field)) =
  "the configuration is invalid: " <> field <> " is negative"
renderParseError (LossyNumberLiteral n) =
  "the number " <> showNumber n <> " is not exactly representable as a double"

-- | A one-line description of an evaluation error, without its location.
renderEvalError :: EvalError -> Text
renderEvalError (UnknownOperator (OperatorName name)) =
  "unknown operator " <> quoted name
renderEvalError (CustomOperatorFailed (OperatorName name) reason) =
  "custom operator " <> quoted name <> " failed: " <> errorMessage reason
renderEvalError (InvalidOperand (OperatorName name) issue) =
  "operator "
    <> quoted name
    <> ": operand "
    <> shown (operandIndex issue + 1)
    <> " must be "
    <> operandExpected issue
    <> ", but is "
    <> operandActual issue
renderEvalError (LossyNumber n) =
  "the number " <> showNumber n <> " is not exactly representable as a double"
renderEvalError (LimitExceeded kind) = case kind of
  StepLimit n -> "evaluation exceeded the limit of " <> shown n <> " steps"
  StringLimit n -> "a string result exceeded the limit of " <> shown n <> " code units"
  ArrayLimit n -> "an array result exceeded the limit of " <> shown n <> " elements"
  IterationLimit n -> "an array to iterate exceeded the limit of " <> shown n <> " elements"
  ValueDepthLimit n -> "a value nests deeper than the limit of " <> shown n
  DataDepthLimit n -> "the data nests deeper than the limit of " <> shown n

-- | A number as it was written, in plain digits unless that would be absurdly long.
showNumber :: Scientific -> Text
showNumber n
  | abs (base10Exponent n) <= 30 = Text.pack (formatScientific Fixed Nothing n)
  | otherwise = Text.pack (formatScientific Exponent Nothing n)

didYouMean :: [OperatorName] -> Text
didYouMean [] = ""
didYouMean near = "; did you mean " <> Text.intercalate " or " [quoted name | OperatorName name <- near] <> "?"

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

-- | A stable, machine-readable tag for each kind of error: the constructor
-- name in @snake_case@. Programs should match on this, not on messages.
errorKind :: Error -> Text
errorKind (ParseFailure _ problem) = case problem of
  DepthExceeded _ -> "depth_exceeded"
  ArityMismatch {} -> "arity_mismatch"
  NodeLimitExceeded _ -> "node_limit_exceeded"
  UnknownOperatorName {} -> "unknown_operator"
  InvalidConfig _ -> "invalid_config"
  LossyNumberLiteral _ -> "lossy_number"
errorKind (EvalFailure _ problem) = case problem of
  UnknownOperator _ -> "unknown_operator"
  CustomOperatorFailed {} -> "custom_operator_failed"
  InvalidOperand {} -> "invalid_operand"
  LimitExceeded _ -> "limit_exceeded"
  LossyNumber _ -> "lossy_number"

-- | @{"kind": ..., "stage": "parse" | "eval", "pointer": RFC 6901,
-- "message": ..., "detail": {...}}@.
instance ToJSON Error where
  toJSON e =
    object
      [ "kind" .= errorKind e
      , "stage" .= (stage :: Text)
      , "pointer" .= renderLocation (errorLocation e)
      , "message" .= renderError e
      , "detail" .= detail
      ]
    where
      (stage, detail) = case e of
        ParseFailure _ problem -> ("parse", parseDetail problem)
        EvalFailure _ problem -> ("eval", evalDetail problem)

parseDetail :: ParseError -> Value
parseDetail problem = case problem of
  DepthExceeded n -> object ["limit" .= n]
  ArityMismatch (OperatorName name) accepted given ->
    object ["operator" .= name, "accepts" .= arityJson accepted, "given" .= given]
  NodeLimitExceeded n -> object ["limit" .= n]
  UnknownOperatorName (OperatorName name) near ->
    object ["operator" .= name, "suggestions" .= [n | OperatorName n <- near]]
  InvalidConfig (NegativeLimit field) -> object ["field" .= field]
  LossyNumberLiteral n -> object ["number" .= n]

evalDetail :: EvalError -> Value
evalDetail problem = case problem of
  UnknownOperator (OperatorName name) -> object ["operator" .= name]
  CustomOperatorFailed (OperatorName name) reason ->
    object ["operator" .= name, "code" .= errorCode reason, "message" .= errorMessage reason, "details" .= errorDetails reason]
  InvalidOperand (OperatorName name) issue ->
    object
      [ "operator" .= name
      , "operand" .= operandIndex issue
      , "expected" .= operandExpected issue
      , "actual" .= operandActual issue
      ]
  LimitExceeded kind -> case kind of
    StepLimit n -> limit "steps" n
    StringLimit n -> limit "string_length" n
    ArrayLimit n -> limit "array_length" n
    IterationLimit n -> limit "iterations" n
    ValueDepthLimit n -> limit "value_depth" n
    DataDepthLimit n -> limit "data_depth" n
  LossyNumber n -> object ["number" .= n]
  where
    limit name n = object ["limit" .= (name :: Text), "value" .= n]

arityJson :: Arity -> Value
arityJson (Exactly n) = object ["exactly" .= n]
arityJson (AtLeast n) = object ["at_least" .= n]
arityJson (Between low high) = object ["between" .= [low, high]]

-- | So that an 'Error' can be thrown, for callers that prefer exceptions.
-- Its 'displayException' is 'renderError', so an uncaught error reads as a
-- sentence and not as a constructor tree.
instance Exception Error where
  displayException = Text.unpack . renderError

instance NFData Arity where
  rnf (Exactly n) = rnf n
  rnf (AtLeast n) = rnf n
  rnf (Between low high) = rnf low `seq` rnf high

instance NFData CustomError where
  rnf (CustomError code message details) = rnf code `seq` rnf message `seq` rnf details

instance NFData OperandIssue where
  rnf (OperandIssue index expected actual) = rnf index `seq` rnf expected `seq` rnf actual

instance NFData ParseError where
  rnf (DepthExceeded n) = rnf n
  rnf (ArityMismatch name arity given) = rnf name `seq` rnf arity `seq` rnf given
  rnf (NodeLimitExceeded n) = rnf n
  rnf (UnknownOperatorName name near) = rnf name `seq` rnf near
  rnf (InvalidConfig e) = rnf e
  rnf (LossyNumberLiteral n) = rnf n

instance NFData LimitKind where
  rnf (StepLimit n) = rnf n
  rnf (StringLimit n) = rnf n
  rnf (ArrayLimit n) = rnf n
  rnf (IterationLimit n) = rnf n
  rnf (ValueDepthLimit n) = rnf n
  rnf (DataDepthLimit n) = rnf n

instance NFData EvalError where
  rnf (UnknownOperator name) = rnf name
  rnf (CustomOperatorFailed name reason) = rnf name `seq` rnf reason
  rnf (InvalidOperand name issue) = rnf name `seq` rnf issue
  rnf (LimitExceeded kind) = rnf kind
  rnf (LossyNumber n) = rnf n

instance NFData OperatorsError where
  rnf (ReservedOperatorName name) = rnf name
  rnf (DuplicateOperatorName name) = rnf name
  rnf (InvalidOperatorName name) = rnf name

instance NFData Error where
  rnf (ParseFailure location problem) = rnf location `seq` rnf problem
  rnf (EvalFailure location problem) = rnf location `seq` rnf problem

