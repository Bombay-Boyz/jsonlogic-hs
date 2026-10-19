{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : JsonLogic.Core.Parse
--
-- Reads a JSON 'Value' into a 'Rule'. This is an anamorphism (an unfold):
-- the seed is a JSON value plus how deep we already are, and each step
-- produces one layer of 'RuleF' whose children are further seeds.
--
-- Reading follows the reference implementation's own notion of \"is this
-- a rule\": an object with exactly one key is an operator call; an array
-- is a node whose elements are each read as rules; anything else
-- (including an object with zero or several keys) is a literal.
--
-- __Arity.__ The reference implementation silently ignores extra arguments
-- and pads missing ones. In 'Strict' mode (the default) a wrong argument
-- count is a 'ParseError', because 'RuleF' makes those shapes
-- unrepresentable (Standard 1.2). In 'Lenient' mode the count is repaired
-- here, at parse time, exactly as the reference would behave: missing
-- arguments become JavaScript @undefined@ (an empty @and@), and surplus
-- ones are dropped, after being evaluated for their errors when the
-- reference evaluates them ('Discard').
module JsonLogic.Core.Parse
  ( parseRule
  , parseRuleWithDepth
  , parseRuleWith
  , defaultMaxDepth
  , builtinOperatorNames
  ) where

import Data.Aeson (Value (..), toJSON)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Fix (Fix (..))
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)

import JsonLogic.Core.Config (ArityMode (..), Config (..), defaultConfig)
import JsonLogic.Core.Errors (Arity (..), ParseError (..))
import JsonLogic.Core.Rule (OperatorName (..), Rule, RuleF (..))
import JsonLogic.Core.Value (nestsDeeperThan)

-- | The unfold's seed: nesting depth so far, and the JSON still to read.
type Seed = (Int, Value)

-- | Nesting limit used by 'parseRule'.
defaultMaxDepth :: Int
defaultMaxDepth = maxDepth defaultConfig

-- | Read a rule using 'defaultConfig'.
parseRule :: Value -> Either ParseError Rule
parseRule = parseRuleWith defaultConfig

-- | Read a rule in strict mode, rejecting any nesting deeper than the
-- given limit. Nesting of exactly @limit@ levels is accepted; one more is
-- not.
parseRuleWithDepth :: Int -> Value -> Either ParseError Rule
parseRuleWithDepth limit = parseRuleWith defaultConfig{maxDepth = limit}

-- | Read a rule with the given depth limit and arity mode.
parseRuleWith :: Config -> Value -> Either ParseError Rule
parseRuleWith config value = anaM (step (maxDepth config) (arityMode config)) (0, value)

-- | Monadic anamorphism: unfold a seed into a @Fix@, stopping at the
-- first failure. Defined locally because the library version could not
-- be confirmed available; swap for it if it exists (Standard 4.7).
anaM :: (Monad m, Traversable f) => (a -> m (f a)) -> a -> m (Fix f)
anaM coalgebra = go
  where
    go seed = coalgebra seed >>= fmap Fix . traverse go

-- | One layer of the unfold.
step :: Int -> ArityMode -> Seed -> Either ParseError (RuleF Seed)
step limit mode (depth, value)
  | depth > limit = Left (DepthExceeded limit)
  | otherwise = case value of
      Array items -> Right (Arr (map (deeper depth) (toList items)))
      Object fields -> case KeyMap.toList fields of
        [(key, args)] -> operator mode depth (Key.toText key) (arguments args)
        _ -> literal limit depth value
      _ -> literal limit depth value

-- | A literal's own nesting counts towards the depth limit, so a deeply
-- nested object cannot be smuggled past it inside a rule.
literal :: Int -> Int -> Value -> Either ParseError (RuleF Seed)
literal limit depth value
  | nestsDeeperThan (limit - depth) value = Left (DepthExceeded limit)
  | otherwise = Right (Lit value)

deeper :: Int -> Value -> Seed
deeper depth value = (depth + 1, value)

-- | An operator's argument, or argument list, as a list. A bare value is
-- shorthand for a one-element list.
arguments :: Value -> [Value]
arguments (Array items) = toList items
arguments other = [other]

-- | The reference's JavaScript @undefined@, as a rule: an empty @and@.
undefinedRule :: Value
undefinedRule = Object (KeyMap.singleton "and" (toJSON ([] :: [Value])))

-- | How an operator with a bounded argument count treats it: the fewest and
-- most it takes, and whether the reference evaluates surplus operands
-- (@True@) or never looks at them (@False@: @map@, @reduce@ and the other
-- operators the reference evaluates lazily). Operators that take any
-- number of arguments have no shape.
data Shape = Fixed Int Int Bool

shapeOf :: Text -> Maybe Shape
shapeOf name = case name of
  "var" -> Just (Fixed 0 2 True)
  "missing_some" -> Just (Fixed 2 2 True)
  "!" -> Just (Fixed 1 1 True)
  "!!" -> Just (Fixed 1 1 True)
  "==" -> Just (Fixed 2 2 True)
  "===" -> Just (Fixed 2 2 True)
  "!=" -> Just (Fixed 2 2 True)
  "!==" -> Just (Fixed 2 2 True)
  "<" -> Just (Fixed 2 3 True)
  "<=" -> Just (Fixed 2 3 True)
  ">" -> Just (Fixed 2 2 True)
  ">=" -> Just (Fixed 2 2 True)
  "-" -> Just (Fixed 1 2 True)
  "/" -> Just (Fixed 2 2 True)
  "%" -> Just (Fixed 2 2 True)
  "map" -> Just (Fixed 2 2 False)
  "filter" -> Just (Fixed 2 2 False)
  "reduce" -> Just (Fixed 3 3 False)
  "all" -> Just (Fixed 2 2 False)
  "none" -> Just (Fixed 2 2 False)
  "some" -> Just (Fixed 2 2 False)
  "in" -> Just (Fixed 2 2 True)
  "substr" -> Just (Fixed 2 3 True)
  "log" -> Just (Fixed 1 1 True)
  _ -> Nothing

-- | The names the parser maps to builtin operators. A custom operator may
-- not use one of these names, because the builtin would shadow it.
builtinOperatorNames :: [Text]
builtinOperatorNames =
  [ "var"
  , "missing"
  , "missing_some"
  , "if"
  , "?:"
  , "and"
  , "or"
  , "!"
  , "!!"
  , "=="
  , "==="
  , "!="
  , "!=="
  , "<"
  , "<="
  , ">"
  , ">="
  , "max"
  , "min"
  , "+"
  , "-"
  , "*"
  , "/"
  , "%"
  , "map"
  , "filter"
  , "reduce"
  , "all"
  , "none"
  , "some"
  , "merge"
  , "in"
  , "cat"
  , "substr"
  , "log"
  ]

-- | Repair an argument list to an operator's arity, in lenient mode.
-- Returns the list the operator can be built from, plus the surplus
-- operands that are still to be evaluated.
repair :: Text -> [Value] -> ([Value], [Value])
repair name args = case shapeOf name of
  Just (Fixed low high evaluated)
    | length args < low && name /= "var" -> (args ++ padding name (length args) low, [])
    | length args > high -> (take high args, if evaluated then drop high args else [])
  _ -> (args, [])

-- | What stands in for a missing operand: @undefined@, except for the
-- initial value of @reduce@, which the reference defaults to @null@.
padding :: Text -> Int -> Int -> [Value]
padding name have want =
  [ if name == "reduce" && i == 2 then Null else undefinedRule
  | i <- [have .. want - 1]
  ]

-- | The operator table. Names outside the closed builtin set become
-- 'CustomOp', whose behaviour is supplied by the caller at evaluation.
operator :: ArityMode -> Int -> Text -> [Value] -> Either ParseError (RuleF Seed)
operator mode depth name args = case mode of
  Strict -> build (map (deeper depth) args)
  Lenient -> case repair name args of
    (fixed, []) -> build (map (deeper depth) fixed)
    (fixed, extras) ->
      -- The repaired call is read again as an ordinary one, at the same
      -- depth: it has the right number of operands, so it cannot loop.
      Right
        ( Discard
            (depth, Object (KeyMap.singleton (Key.fromText name) (toJSON fixed)))
            (map (deeper depth) extras)
        )
  where
    build = operatorFrom name depth

operatorFrom :: Text -> Int -> [Seed] -> Either ParseError (RuleF Seed)
operatorFrom name depth args = case name of
  "var" -> oneOrTwo op Var (varOperands depth args)
  "missing" -> anyNumber Missing args
  "missing_some" -> exactlyTwo op MissingSome args
  "if" -> anyNumber If args
  "?:" -> anyNumber If args
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
  "max" -> anyNumber Max args
  "min" -> anyNumber Min args
  "+" -> anyNumber Add args
  "-" -> oneOrTwo op Sub args
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

-- | The classic suite requires @{\"var\": []}@ to mean the data itself, like
-- @{\"var\": null}@ and @{\"var\": \"\"}@, so an empty argument list is read as
-- a single null path rather than rejected for its arity.
varOperands :: Int -> [Seed] -> [Seed]
varOperands depth [] = [(depth + 1, Null)]
varOperands _ args = args

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
