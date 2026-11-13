{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : JsonLogic.Core.Check
--
-- Problems with a rule that depend on the registry of custom operators, and
-- so cannot be found by reading the rule alone: an operator nobody
-- registered (with a suggestion for what was probably meant), and a
-- registered operator given the wrong number of operands.
module JsonLogic.Core.Check
  ( customProblem
  , unknownOperatorsIn
  , suggest
  ) where

import Data.Fix (foldFix)
import Data.Foldable (fold)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as Text

import JsonLogic.Core.Config (ArityMode (..), Config (..), rejectsUnknownOperators)
import JsonLogic.Core.Custom (Operators, lookupOperator, operatorArity, operatorNames)
import JsonLogic.Core.Errors (Arity (..), ParseError (..))
import JsonLogic.Core.Location (Location)
import JsonLogic.Core.Operator (Builtin, OperatorName (..), allBuiltins, builtinName)
import JsonLogic.Core.Rule (Node (..), RuleF (..), RuleTree)

-- | What is wrong with a call to a non-builtin operator given this many
-- operands, if anything. An unknown operator is a problem only when the
-- configuration refuses them at compile time; the arity of a registered
-- operator is checked only in strict arity mode.
customProblem :: Config -> Operators -> OperatorName -> Int -> Maybe ParseError
customProblem config operators name@(OperatorName text) count =
  case lookupOperator operators name of
    Nothing
      | rejectsUnknownOperators config -> Just (UnknownOperatorName name (suggest operators text))
      | otherwise -> Nothing
    Just implementation
      | arityMode config == Strict
      , not (admits (operatorArity implementation) count) ->
          Just (ArityMismatch name (operatorArity implementation) count)
      | otherwise -> Nothing
  where
    admits (Exactly n) c = c == n
    admits (AtLeast n) c = c >= n
    admits (Between low high) c = c >= low && c <= high

-- | Every operator in a rule that is neither builtin nor registered, with
-- where it is, in the order they appear.
unknownOperatorsIn :: Operators -> RuleTree -> [(Location, OperatorName)]
unknownOperatorsIn operators = foldFix layer
  where
    layer (Node location node) = own <> fold node
      where
        own = case node of
          CustomOp name _ | isNothing (lookupOperator operators name) -> [(location, name)]
          _ -> []

-- | The known operator names nearest to a name: within two edits, and no
-- farther than the nearest one found, at most three. Builtins and registered
-- operators are both known. Empty when nothing is close.
suggest :: Operators -> Text -> [OperatorName]
suggest operators name = case sortOn id close of
  [] -> []
  ranked@((best, _) : _) -> take 3 [OperatorName candidate | (d, candidate) <- ranked, d == best]
  where
    close = [(d, candidate) | candidate <- known, let d = editDistance name candidate, d <= 2, d < Text.length name]
    known = map builtinName (allBuiltins :: [Builtin]) ++ [n | OperatorName n <- operatorNames operators]

-- | Levenshtein distance, by the usual table kept one row at a time. Total:
-- no partial function, and the rows are never empty by construction. Uses
-- only what every supported GHC's Prelude has, and the strings are operator
-- names, so a lazy fold costs nothing.
editDistance :: Text -> Text -> Int
editDistance a b = lastOf (foldl row [0 .. length as] (Text.unpack b))
  where
    as = Text.unpack a
    -- the row for the prefix of @b@ read so far plus @c@, from the row before
    row previous c = case previous of
      [] -> []
      corner : rest -> scanl (compute c) (corner + 1) (zip3 as previous rest)
    compute c left (ac, diagonal, up) =
      minimum (left + 1 :| [up + 1, diagonal + (if ac == c then 0 else 1)])
    lastOf = foldl (\_ x -> x) 0
