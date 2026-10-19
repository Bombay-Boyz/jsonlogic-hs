-- |
-- Module      : JsonLogic.Core.Config
--
-- Configuration for reading and evaluating rules: the resource limits that
-- make it safe to evaluate rules from an untrusted source, and the arity
-- mode.
--
-- There is no global state. A @Config@ is an ordinary value passed to
-- 'JsonLogic.compile' or 'JsonLogic.applyWithConfig'.
module JsonLogic.Core.Config
  ( ArityMode (..)
  , Config (..)
  , defaultConfig
  ) where

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

-- | Limits and mode. All limits bound /work and memory/, not wall-clock
-- time: a custom operator that is slow is still slow.
data Config = Config
  { maxDepth :: !Int
  -- ^ How deeply a rule (and any literal inside it, and any value a custom
  -- operator returns) may nest. Default 64.
  , maxSteps :: !Int
  -- ^ How many evaluation steps one evaluation may take. A step is one
  -- rule node evaluated, one iteration of @map@, @filter@, @reduce@,
  -- @all@, @some@ or @none@, or one element or character built by @cat@ or
  -- @merge@. Default 1,000,000.
  , maxStringLength :: !Int
  -- ^ The longest string @cat@ may build, in UTF-16 code units. Default
  -- 1,048,576.
  , maxArrayLength :: !Int
  -- ^ The longest array @merge@, @map@ or @filter@ may build. Default
  -- 100,000.
  , arityMode :: !ArityMode
  -- ^ 'Strict' (the default) or 'Lenient'.
  }
  deriving (Eq, Show)

-- | The default configuration: depth 64, one million steps, 1 MiB strings
-- (in code units), 100,000-element arrays, strict arity.
defaultConfig :: Config
defaultConfig =
  Config
    { maxDepth = 64
    , maxSteps = 1000000
    , maxStringLength = 1048576
    , maxArrayLength = 100000
    , arityMode = Strict
    }
