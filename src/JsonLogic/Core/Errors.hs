-- |
-- Module      : JsonLogic.Core.Errors
--
-- The closed error vocabulary of the pipeline, layered by stage
-- (VISION.md §7). Only the parsing layer exists so far; validation and
-- evaluation errors are added when those stages are written, not before
-- (Standard 2.11).
module JsonLogic.Core.Errors
  ( Arity (..)
  , ParseError (..)
  ) where

import JsonLogic.Core.Rule (OperatorName)

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
    -- admit: operator, what it accepts, how many it was given.
    ArityMismatch OperatorName Arity Int
  deriving (Eq, Show)
