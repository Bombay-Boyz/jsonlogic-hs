{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : JsonLogic.Core.Config
--
-- Configuration for reading and evaluating rules: the resource limits that
-- make it safe to evaluate rules from an untrusted source, the arity mode,
-- how unknown operators and lossy numbers are treated.
--
-- There is no global state. A @Config@ is an ordinary value passed to
-- 'JsonLogic.compile' or 'JsonLogic.applyWithConfig'.
--
-- The 'Config' constructor is not exported: build one from 'defaultConfig'
-- with record update (@defaultConfig {maxSteps = 10000}@), so that adding a
-- field is never a breaking change. 'validateConfig' (called by every entry
-- point) refuses a configuration that cannot mean anything, such as a
-- negative limit.
module JsonLogic.Core.Config
  ( ArityMode (..)
  , UnknownOperators (..)
  , NumericSafety (..)
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
  , ConfigError (..)
  , defaultConfig
  , unlimited
  , validateConfig
  , rejectsUnknownOperators
  ) where

import Control.DeepSeq (NFData (..))
import Data.Text (Text)

-- | What to do with an operator given the wrong number of arguments.
data ArityMode
  = -- | Reject it with a precise 'JsonLogic.ArityMismatch'. The default:
    -- a rule that passes the wrong number of arguments is almost always a
    -- mistake, and the reference silently hides it.
    Strict
  | -- | Behave like the reference implementation: ignore surplus
    -- arguments (after still evaluating them, as the reference does) and
    -- treat missing ones as JavaScript @undefined@.
    Lenient
  deriving (Eq, Show)

-- | What to do with a rule that names an operator that is neither builtin
-- nor registered.
data UnknownOperators
  = -- | 'RejectAtCompile' in 'Strict' arity mode and 'FailWhenReached' in
    -- 'Lenient' mode, so that each mode is internally consistent. The
    -- default.
    FollowArityMode
  | -- | Refuse the rule when it is compiled, naming the operator, where it
    -- is, and the nearest known operators.
    RejectAtCompile
  | -- | Accept the rule and fail only if a run reaches the operator, as the
    -- reference does (it evaluates rules lazily).
    FailWhenReached
  deriving (Eq, Show)

-- | How numbers that a JavaScript double cannot hold exactly are treated.
-- Such a number is an integer above 2^53, or a decimal whose nearest
-- double does not print back as the same digits.
data NumericSafety
  = -- | The reference's behaviour: every number is read as a double, and
    -- printed as the shortest digits that read back as it. A value that
    -- merely passes through a rule can change (@9007199254740993@ comes out
    -- as @9007199254740992@). The default.
    JsNumbers
  | -- | Arithmetic and comparison use doubles as in 'JsNumbers', but a
    -- number that reaches the result unchanged keeps its original digits.
    PreserveSource
  | -- | Refuse such a number, in the rule or the data, with a
    -- 'JsonLogic.LossyNumber' error instead of rounding it.
    RejectLossy
  deriving (Eq, Show)

-- | Limits and modes. All limits bound /work and memory/, not wall-clock
-- time: a custom operator that is slow is still slow. A limit of
-- 'unlimited' means no limit; a negative limit is refused by
-- 'validateConfig'.
data Config = Config
  { maxDepth :: !Int
  -- ^ How deeply a rule (and any literal inside it, and any value a custom
  -- operator returns) may nest. Default 64.
  , maxSteps :: !Int
  -- ^ How many evaluation steps one run may take. A step is one rule node
  -- evaluated (so each iteration of @map@ and the others costs the nodes of
  -- its body), plus one per array element converted to a string or number,
  -- per code unit or element built by @cat@ or @merge@, per code point of a
  -- string a path indexes or measures, and per node of a custom operator's
  -- result (docs/LIMITS.md). Default 1,000,000.
  , maxStringLength :: !Int
  -- ^ The longest string @cat@ may build, in UTF-16 code units. Default
  -- 1,048,576.
  , maxArrayLength :: !Int
  -- ^ The longest array @merge@, @map@ or @filter@ may /build/. Default
  -- 100,000.
  , maxIterations :: !Int
  -- ^ The longest array @map@, @filter@, @reduce@, @all@, @none@ or @some@
  -- may /iterate/: the same for all six. Separate from 'maxArrayLength',
  -- which bounds what is built, so that admitting a large dataset does not
  -- loosen the guard against amplification. Default 100,000.
  , maxNodes :: !Int
  -- ^ How many nodes the rule's JSON may have: every scalar, array and
  -- object counts one. Checked before the rule is read, so an enormous
  -- rule is refused without being built. Default 1,000,000.
  , maxDataDepth :: !Int
  -- ^ How deeply the data may nest. Enforced by 'JsonLogic.checkData',
  -- which callers use to screen untrusted input once; evaluation itself
  -- does not walk the data. Default 1,000.
  , arityMode :: !ArityMode
  -- ^ 'Strict' (the default) or 'Lenient'.
  , unknownOperators :: !UnknownOperators
  -- ^ 'FollowArityMode' (the default), 'RejectAtCompile' or
  -- 'FailWhenReached'.
  , numericSafety :: !NumericSafety
  -- ^ 'JsNumbers' (the default), 'PreserveSource' or 'RejectLossy'.
  }
  deriving (Eq, Show)

-- | Why a 'Config' was refused.
newtype ConfigError
  = -- | The named field holds a negative limit.
    NegativeLimit Text
  deriving (Eq, Show)

instance NFData ArityMode where
  rnf mode = mode `seq` ()

instance NFData UnknownOperators where
  rnf mode = mode `seq` ()

instance NFData NumericSafety where
  rnf mode = mode `seq` ()

instance NFData ConfigError where
  rnf (NegativeLimit field) = rnf field

instance NFData Config where
  rnf (Config depth steps string array iterations nodes dataDepth mode unknown numeric) =
    depth
      `seq` steps
      `seq` string
      `seq` array
      `seq` iterations
      `seq` nodes
      `seq` dataDepth
      `seq` rnf mode
      `seq` rnf unknown
      `seq` rnf numeric

-- | The default configuration: depth 64, one million steps, 1 MiB strings
-- (in code units), 100,000-element arrays built and iterated, a million rule
-- nodes, data depth 1,000, strict arity, unknown operators refused when the
-- rule is compiled, reference number semantics.
defaultConfig :: Config
defaultConfig =
  Config
    { maxDepth = 64
    , maxSteps = 1000000
    , maxStringLength = 1048576
    , maxArrayLength = 100000
    , maxIterations = 100000
    , maxNodes = 1000000
    , maxDataDepth = 1000
    , arityMode = Strict
    , unknownOperators = FollowArityMode
    , numericSafety = JsNumbers
    }

-- | The value of a limit that is not limited.
unlimited :: Int
unlimited = maxBound

-- | Refuse a configuration that cannot mean anything. Every entry point
-- calls this, so a bad limit is an error and never a silent \"refuse
-- everything\".
validateConfig :: Config -> Either ConfigError Config
validateConfig config = case [name | (name, value) <- limits, value < 0] of
  name : _ -> Left (NegativeLimit name)
  [] -> Right config
  where
    limits =
      [ ("maxDepth", maxDepth config)
      , ("maxSteps", maxSteps config)
      , ("maxStringLength", maxStringLength config)
      , ("maxArrayLength", maxArrayLength config)
      , ("maxIterations", maxIterations config)
      , ("maxNodes", maxNodes config)
      , ("maxDataDepth", maxDataDepth config)
      ]

-- | Whether a configuration refuses unknown operators when a rule is
-- compiled.
rejectsUnknownOperators :: Config -> Bool
rejectsUnknownOperators config = case unknownOperators config of
  RejectAtCompile -> True
  FailWhenReached -> False
  FollowArityMode -> arityMode config == Strict
