{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : JsonLogic.Core.Parse
--
-- Reads a JSON 'Value' into a 'Rule'. This is an anamorphism (an unfold):
-- the seed is a JSON value plus how deep we already are, and each step
-- produces one layer of 'RuleF' whose children are further seeds.
--
-- Reading follows the reference implementation's own notion of "is this
-- a rule": an object with exactly one key is an operator call; an array
-- is a node whose elements are each read as rules; anything else
-- (including an object with zero or several keys) is a literal.
--
-- __Deliberate deviation (recorded in VISION.md §11):__ the reference
-- implementation silently ignores extra arguments and pads missing ones.
-- Here a wrong argument count is a 'ParseError', because 'RuleF' makes
-- those shapes unrepresentable (Standard 1.2).
module JsonLogic.Core.Parse
  ( parseRule
  , parseRuleWithDepth
  , defaultMaxDepth
  ) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Fix (Fix (..))
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)

import JsonLogic.Core.Errors (Arity (..), ParseError (..))
import JsonLogic.Core.Rule (OperatorName (..), Rule, RuleF (..))

-- | The unfold's seed: nesting depth so far, and the JSON still to read.
type Seed = (Int, Value)

-- | Nesting limit used by 'parseRule'. A hostile, deeply nested rule is
-- rejected here rather than exhausting the stack later.
defaultMaxDepth :: Int
defaultMaxDepth = 64

-- | Read a rule using 'defaultMaxDepth'.
parseRule :: Value -> Either ParseError Rule
parseRule = parseRuleWithDepth defaultMaxDepth

-- | Read a rule, rejecting any nesting deeper than the given limit.
-- Nesting of exactly @limit@ levels is accepted; one more is not.
parseRuleWithDepth :: Int -> Value -> Either ParseError Rule
parseRuleWithDepth limit value = anaM (step limit) (0, value)

-- | Monadic anamorphism: unfold a seed into a 'Fix', stopping at the
-- first failure. Defined locally because the library version could not
-- be confirmed available; swap for it if it exists (Standard 4.7).
anaM :: (Monad m, Traversable f) => (a -> m (f a)) -> a -> m (Fix f)
anaM coalgebra = go
  where
    go seed = coalgebra seed >>= fmap Fix . traverse go

-- | One layer of the unfold.
step :: Int -> Seed -> Either ParseError (RuleF Seed)
step limit (depth, value)
  | depth > limit = Left (DepthExceeded limit)
  | otherwise = case value of
      Array items -> Right (Arr (map (deeper depth) (toList items)))
      Object fields -> case KeyMap.toList fields of
        [(key, args)] ->
          operator (Key.toText key) (map (deeper depth) (arguments args))
        _ -> Right (Lit value)
      _ -> Right (Lit value)

deeper :: Int -> Value -> Seed
deeper depth value = (depth + 1, value)

-- | An operator's argument, or argument list, as a list. A bare value is
-- shorthand for a one-element list.
arguments :: Value -> [Value]
arguments (Array items) = toList items
arguments other = [other]

-- | The operator table. Names outside the closed builtin set become
-- 'CustomOp', whose behaviour is supplied by the caller at evaluation.
operator :: Text -> [Seed] -> Either ParseError (RuleF Seed)
operator name args = case name of
  "var" -> oneOrTwo op Var args
  "missing" -> anyNumber Missing args
  "missing_some" -> exactlyTwo op MissingSome args
  "if" -> atLeastOne op If args
  "?:" -> atLeastOne op If args
  "and" -> anyNumber And args
  "or" -> anyNumber Or args
  "!" -> exactlyOne op Not args
  "!!" -> exactlyOne op Cast args
  "==" -> exactlyTwo op LooseEq args
  "===" -> exactlyTwo op StrictEq args
  "!=" -> exactlyTwo op LooseNeq args
  "!==" -> exactlyTwo op StrictNeq args
  "<" -> twoOrThree op Lt args
  "<=" -> twoOrThree op Lte args
  ">" -> exactlyTwo op Gt args
  ">=" -> exactlyTwo op Gte args
  "max" -> atLeastOne op Max args
  "min" -> atLeastOne op Min args
  "+" -> anyNumber Add args
  "-" -> atLeastOne op Sub args
  "*" -> atLeastOne op Mul args
  "/" -> exactlyTwo op Div args
  "%" -> exactlyTwo op Mod args
  "map" -> exactlyTwo op Map args
  "filter" -> exactlyTwo op Filter args
  "reduce" -> exactlyThree op Reduce args
  "all" -> exactlyTwo op All args
  "none" -> exactlyTwo op None args
  "some" -> exactlyTwo op Some args
  "merge" -> anyNumber Merge args
  "in" -> exactlyTwo op In args
  "cat" -> anyNumber Cat args
  "substr" -> twoOrThree op Substr args
  "log" -> exactlyOne op Log args
  _ -> Right (CustomOp op args)
  where
    op = OperatorName name

exactlyOne :: OperatorName -> (a -> RuleF a) -> [a] -> Either ParseError (RuleF a)
exactlyOne _ build [x] = Right (build x)
exactlyOne name _ xs = Left (ArityMismatch name (Exactly 1) (length xs))

exactlyTwo :: OperatorName -> (a -> a -> RuleF a) -> [a] -> Either ParseError (RuleF a)
exactlyTwo _ build [x, y] = Right (build x y)
exactlyTwo name _ xs = Left (ArityMismatch name (Exactly 2) (length xs))

exactlyThree :: OperatorName -> (a -> a -> a -> RuleF a) -> [a] -> Either ParseError (RuleF a)
exactlyThree _ build [x, y, z] = Right (build x y z)
exactlyThree name _ xs = Left (ArityMismatch name (Exactly 3) (length xs))

oneOrTwo :: OperatorName -> (a -> Maybe a -> RuleF a) -> [a] -> Either ParseError (RuleF a)
oneOrTwo _ build [x] = Right (build x Nothing)
oneOrTwo _ build [x, y] = Right (build x (Just y))
oneOrTwo name _ xs = Left (ArityMismatch name (Between 1 2) (length xs))

twoOrThree :: OperatorName -> (a -> a -> Maybe a -> RuleF a) -> [a] -> Either ParseError (RuleF a)
twoOrThree _ build [x, y] = Right (build x y Nothing)
twoOrThree _ build [x, y, z] = Right (build x y (Just z))
twoOrThree name _ xs = Left (ArityMismatch name (Between 2 3) (length xs))

atLeastOne :: OperatorName -> (NonEmpty a -> RuleF a) -> [a] -> Either ParseError (RuleF a)
atLeastOne name build xs = case NonEmpty.nonEmpty xs of
  Just ys -> Right (build ys)
  Nothing -> Left (ArityMismatch name (AtLeast 1) 0)

anyNumber :: ([a] -> RuleF a) -> [a] -> Either ParseError (RuleF a)
anyNumber build = Right . build
