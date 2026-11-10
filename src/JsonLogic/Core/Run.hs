-- |
-- Module      : JsonLogic.Core.Run
--
-- The evaluation monad: a small state-passing monad whose state is the
-- remaining step budget. It is pure: the budget is threaded through the
-- evaluation, never held in a global or in 'IO'. A failure carries the
-- location of the node that failed. See "JsonLogic.Core.Config" for the
-- limits.
module JsonLogic.Core.Run
  ( Result (..)
  , Run (..)
  , At (..)
  , failAt
  , exceeded
  , spend
  ) where

import JsonLogic.Core.Config (Config (..))
import JsonLogic.Core.Errors (EvalError (..), LimitKind (..))
import JsonLogic.Core.Location (Location)

-- | The outcome of a step: its value and the budget left, or where and why
-- it failed. One constructor per outcome, strict, so the budget cannot pile
-- up as a chain of thunks and a step allocates one cell.
data Result a
  = Done !a {-# UNPACK #-} !Int
  | Failed !Location !EvalError

-- | A computation that may fail, at a location in the rule, and that spends
-- steps from a budget.
newtype Run a = Run {unRun :: Int -> Result a}

instance Functor Run where
  fmap f (Run m) = Run $ \budget -> case m budget of
    Done a left -> Done (f a) left
    Failed location e -> Failed location e
  {-# INLINE fmap #-}

instance Applicative Run where
  pure a = Run (Done a)
  {-# INLINE pure #-}
  Run mf <*> Run ma = Run $ \budget -> case mf budget of
    Failed location e -> Failed location e
    Done f left -> case ma left of
      Failed location e -> Failed location e
      Done a left' -> Done (f a) left'
  {-# INLINE (<*>) #-}

instance Monad Run where
  Run m >>= k = Run $ \budget -> case m budget of
    Failed location e -> Failed location e
    Done a left -> unRun (k a) left
  {-# INLINE (>>=) #-}

-- | What every node's code knows about itself: the configuration it was
-- compiled with and where it is in the rule.
data At = At !Config !Location

-- | Fail here.
failAt :: At -> EvalError -> Run a
failAt (At _ location) e = Run (const (Failed location e))

-- | Fail here because a limit was exceeded.
exceeded :: At -> (Config -> LimitKind) -> Run a
exceeded at@(At config _) kind = failAt at (LimitExceeded (kind config))

-- | Spend @n@ steps, or fail here if fewer than @n@ remain.
spend :: At -> Int -> Run ()
spend (At config location) n = Run $ \budget ->
  if n > budget
    then Failed location (LimitExceeded (StepLimit (maxSteps config)))
    else Done () (budget - n)
{-# INLINE spend #-}
