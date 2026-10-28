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

import Control.DeepSeq (NFData (..))

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
-- time: a custom operator that is slow is still slow. Any limit may be any
-- 'Int'; 'maxBound' means no limit.
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
  -- ^ The longest array @merge@, @map@ or @filter@ may build. Default
  -- 100,000.
  , maxNodes :: !Int
  -- ^ How many nodes the rule's JSON may have: every scalar, array and
  -- object counts one. Checked before the rule is read, so an enormous
  -- rule is refused without being built. Default 1,000,000.
  , arityMode :: !ArityMode
  -- ^ 'Strict' (the default) or 'Lenient'.
  }
  deriving (Eq, Show)

instance NFData ArityMode where
  rnf mode = mode `seq` ()

instance NFData Config where
  rnf (Config depth steps string array nodes mode) =
    depth `seq` steps `seq` string `seq` array `seq` nodes `seq` rnf mode

-- | The default configuration: depth 64, one million steps, 1 MiB strings
-- (in code units), 100,000-element arrays, a million rule nodes, strict
-- arity.
defaultConfig :: Config
defaultConfig =
  Config
    { maxDepth = 64
    , maxSteps = 1000000
    , maxStringLength = 1048576
    , maxArrayLength = 100000
    , maxNodes = 1000000
    , arityMode = Strict
    }
