{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : JsonLogic.Core.Eval
--
-- Evaluation: a catamorphism (a fold) over the rule tree.
--
-- The fold's carrier is a /function/ from the data a rule is evaluated
-- against to a result ('Eval'), not a plain value. That is what lets
-- @map@, @filter@, @reduce@ and friends run the same sub-rule once per
-- element against a different datum, and what makes @if@, @and@ and @or@
-- short-circuit for free: a branch not taken is a function never applied
-- (VISION.md §4, §6).
--
-- Operand evaluation order follows the reference: everything is
-- evaluated before its operator runs, except where the reference
-- evaluates lazily (@if@, @and@, @or@) or against a scoped datum
-- (@map@, @filter@, @reduce@, @all@, @none@, @some@).
module JsonLogic.Core.Eval
  ( CustomOperator
  , Operators
  , evaluate
  , evaluateWith
  , noOperators
  , operatorsFromList
  ) where

import Control.Monad (filterM, foldM)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Char (digitToInt, isDigit)
import Data.Functor.Foldable (cata)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

import JsonLogic.Core.Errors (EvalError (..))
import JsonLogic.Core.Rule (OperatorName, Rule, RuleF (..))
import JsonLogic.Core.Value

-- | A rule compiled to a function of the data it is evaluated against.
type Eval = Val -> Either EvalError Val

-- | A caller-supplied operator: given the data being evaluated against
-- and its already-evaluated arguments, produce a result or a reason.
type CustomOperator = Aeson.Value -> [Aeson.Value] -> Either Text Aeson.Value

-- | The operators available beyond the builtin set (Standard 0.5: a
-- Handle, supplied at the one place that needs it).
newtype Operators = Operators {lookupOperator :: OperatorName -> Maybe CustomOperator}

-- | No custom operators: any operator outside the builtin set is an error.
noOperators :: Operators
noOperators = Operators (const Nothing)

-- | Custom operators from an association list.
operatorsFromList :: [(OperatorName, CustomOperator)] -> Operators
operatorsFromList entries = Operators (`lookup` entries)

-- | Evaluate a rule against some data, with no custom operators.
evaluate :: Rule -> Aeson.Value -> Either EvalError Aeson.Value
evaluate = evaluateWith noOperators

-- | Evaluate a rule against some data.
evaluateWith :: Operators -> Rule -> Aeson.Value -> Either EvalError Aeson.Value
evaluateWith operators rule input =
  toValue <$> cata (algebra operators) rule (fromValue input)

-- | One layer of the fold: given each child already compiled to an
-- 'Eval', compile this node.
algebra :: Operators -> RuleF Eval -> Eval
algebra operators node = case node of
  Lit literal -> const (Right (fromValue literal))
  Arr items -> \env -> VArr <$> evalAll items env
  Var path fallback -> \env -> do
    key <- path env
    notFound <- maybe (Right VNull) ($ env) fallback
    Right (lookupVariable env key notFound)
  Missing keys -> \env -> do
    names <- evalAll keys env
    Right (VArr (missingKeys names env))
  MissingSome needed options -> \env -> do
    count <- needed env
    listed <- options env
    Right (missingSome count listed env)
  If branches -> evalIf branches
  And operands -> evalAnd operands
  Or operands -> evalOr operands
  Not operand -> \env -> VBool . not . truthy <$> operand env
  Cast operand -> \env -> VBool . truthy <$> operand env
  LooseEq x y -> binary (\a b -> VBool (looseEquals a b)) x y
  StrictEq x y -> binary (\a b -> VBool (strictEquals a b)) x y
  LooseNeq x y -> binary (\a b -> VBool (not (looseEquals a b))) x y
  StrictNeq x y -> binary (\a b -> VBool (not (strictEquals a b))) x y
  Lt x y z -> between lessThan x y z
  Lte x y z -> between lessOrEqual x y z
  Gt x y -> binary (\a b -> VBool (lessThan b a)) x y
  Gte x y -> binary (\a b -> VBool (lessOrEqual b a)) x y
  Max operands -> \env -> VNum . maximumJs . map toNumber <$> evalAll operands env
  Min operands -> \env -> VNum . minimumJs . map toNumber <$> evalAll operands env
  Add operands -> \env -> VNum . foldl (\total v -> total + parseFloat v) 0 <$> evalAll operands env
  Sub x y -> \env -> do
    a <- x env
    b <- traverse ($ env) y
    Right (VNum (maybe (negate (toNumber a)) (\v -> toNumber a - toNumber v) b))
  Mul operands -> \env -> multiply <$> evalAll operands env
  Div x y -> binary (\a b -> VNum (toNumber a / toNumber b)) x y
  Mod x y -> binary (\a b -> VNum (remainderJs (toNumber a) (toNumber b))) x y
  Map collection logic ->
    withCollection collection (VArr []) (fmap VArr . traverse logic)
  Filter collection logic ->
    withCollection collection (VArr []) (fmap VArr . filterM (fmap truthy . logic))
  Reduce collection logic initial -> reduceRule collection logic initial
  All collection logic ->
    withCollection collection (VBool False) $ \items ->
      if null items then Right (VBool False) else VBool <$> allTruthy logic items
  None collection logic ->
    withCollection collection (VBool True) $ \items ->
      if null items then Right (VBool True) else VBool . not <$> anyTruthy logic items
  Some collection logic ->
    withCollection collection (VBool False) $ \items ->
      if null items then Right (VBool False) else VBool <$> anyTruthy logic items
  Merge operands -> \env -> VArr . concatMap flattenOnce <$> evalAll operands env
  In needle haystack -> binary (\a b -> VBool (contains a b)) needle haystack
  Cat operands -> \env -> VStr . Text.concat . map toJoinText <$> evalAll operands env
  Substr source start len -> \env -> do
    text <- source env
    from <- start env
    limit <- traverse ($ env) len
    Right (VStr (substring (toJsString text) from limit))
  Log operand -> operand
  CustomOp name operands -> \env -> do
    arguments <- evalAll operands env
    case lookupOperator operators name of
      Nothing -> Left (UnknownOperator name)
      Just implementation ->
        case implementation (toValue env) (map toValue arguments) of
          Left reason -> Left (CustomOperatorFailed name reason)
          Right result -> Right (fromValue result)

-- ---------------------------------------------------------------------
-- Combinators over compiled operands
-- ---------------------------------------------------------------------

evalAll :: Traversable t => t Eval -> Val -> Either EvalError (t Val)
evalAll operands env = traverse ($ env) operands

binary :: (Val -> Val -> Val) -> Eval -> Eval -> Eval
binary combine x y env = combine <$> x env <*> y env

-- | @x op y@, or the chained @x op y && y op z@ when a third operand is
-- given. All operands are evaluated first, as in the reference.
between :: (Val -> Val -> Bool) -> Eval -> Eval -> Maybe Eval -> Eval
between holds x y z env = do
  a <- x env
  b <- y env
  c <- traverse ($ env) z
  Right (VBool (maybe (holds a b) (\third -> holds a b && holds b third) c))

-- | Condition/consequent pairs, then an optional else; no branch means null.
evalIf :: [Eval] -> Eval
evalIf (condition : consequent : rest) env = do
  decided <- condition env
  if truthy decided then consequent env else evalIf rest env
evalIf [fallback] env = fallback env
evalIf [] _ = Right VNull

-- | The first falsy operand, or the last; null when there are none.
evalAnd :: [Eval] -> Eval
evalAnd [] _ = Right VNull
evalAnd [only] env = only env
evalAnd (first : rest) env = do
  value <- first env
  if truthy value then evalAnd rest env else Right value

-- | The first truthy operand, or the last; null when there are none.
evalOr :: [Eval] -> Eval
evalOr [] _ = Right VNull
evalOr [only] env = only env
evalOr (first : rest) env = do
  value <- first env
  if truthy value then Right value else evalOr rest env

-- | Evaluate a collection operand; run @onItems@ if it is an array,
-- otherwise answer @fallback@.
withCollection :: Eval -> Val -> ([Val] -> Either EvalError Val) -> Eval
withCollection collection fallback onItems env = do
  evaluated <- collection env
  case evaluated of
    VArr items -> onItems items
    _ -> Right fallback

-- | Fold the array with the sub-rule, evaluated against an object holding
-- the current element and the accumulator.
reduceRule :: Eval -> Eval -> Eval -> Eval
reduceRule collection logic initial env = do
  evaluated <- collection env
  start <- initial env
  case evaluated of
    VArr items -> foldM step start items
    _ -> Right start
  where
    step accumulator item =
      logic (VObj (KeyMap.fromList [("current", item), ("accumulator", accumulator)]))

allTruthy :: Eval -> [Val] -> Either EvalError Bool
allTruthy _ [] = Right True
allTruthy logic (item : rest) = do
  value <- logic item
  if truthy value then allTruthy logic rest else Right False

anyTruthy :: Eval -> [Val] -> Either EvalError Bool
anyTruthy _ [] = Right False
anyTruthy logic (item : rest) = do
  value <- logic item
  if truthy value then Right True else anyTruthy logic rest

-- ---------------------------------------------------------------------
-- Operator semantics
-- ---------------------------------------------------------------------

-- | The reference folds @*@ with no seed: a single operand is returned
-- untouched (not converted to a number), two or more are parsed and
-- multiplied.
multiply :: NonEmpty Val -> Val
multiply (first :| rest) =
  foldl (\acc v -> VNum (parseFloat acc * parseFloat v)) first rest

-- | @merge@ concatenates, flattening one level; non-arrays are appended.
flattenOnce :: Val -> [Val]
flattenOnce (VArr items) = items
flattenOnce other = [other]

-- | @in@: substring of a string, or strict-equal member of an array.
-- A falsy container (including the empty string) contains nothing.
contains :: Val -> Val -> Bool
contains _ (VStr s) | Text.null s = False
contains needle (VStr s) = Text.isInfixOf (toJsString needle) s
contains needle (VArr items) = any (strictEquals needle) items
contains _ _ = False

-- | Which of the keys have no value, or an empty-string one. If the first
-- argument is itself an array, it is the list of keys.
missingKeys :: [Val] -> Val -> [Val]
missingKeys args dat = filter isMissing keys
  where
    keys = case args of
      VArr listed : _ -> listed
      _ -> args
    isMissing key = case lookupVariable dat key VNull of
      VNull -> True
      VStr s -> Text.null s
      _ -> False

-- | @missing_some@: nothing is missing if at least @count@ of the options
-- are present; otherwise the missing ones. A non-array option list is
-- treated as empty.
missingSome :: Val -> Val -> Val -> Val
missingSome count options env
  | lessOrEqual count (VNum (fromIntegral (length names - length absent))) = VArr []
  | otherwise = VArr absent
  where
    names = case options of
      VArr items -> items
      _ -> []
    absent = missingKeys names env

-- | @var@: look a dotted path up in the data. An empty or null path is
-- the data itself; a missing step gives the fallback.
lookupVariable :: Val -> Val -> Val -> Val
lookupVariable dat path notFound
  | isEmptyPath path = dat
  | otherwise = fromMaybe notFound (descend dat (Text.splitOn "." (toJsString path)))
  where
    isEmptyPath VNull = True
    isEmptyPath (VStr s) = Text.null s
    isEmptyPath _ = False

-- | Follow the path one key at a time; 'Nothing' is "no such property".
descend :: Val -> [Text] -> Maybe Val
descend current [] = Just current
descend VNull _ = Nothing
descend current (key : keys) = property current key >>= \child -> descend child keys

-- | JavaScript property access on a JSON value: object keys, array and
-- string indices, and @length@.
property :: Val -> Text -> Maybe Val
property (VObj fields) key = KeyMap.lookup (Key.fromText key) fields
property (VArr items) key
  | key == "length" = Just (VNum (fromIntegral (length items)))
  | otherwise = arrayIndex key >>= \i -> listToMaybe (drop i items)
property (VStr s) key
  | key == "length" = Just (VNum (fromIntegral (Text.length s)))
  | otherwise = arrayIndex key >>= \i -> characterAt i s
property _ _ = Nothing

characterAt :: Int -> Text -> Maybe Val
characterAt i s
  | Text.null picked = Nothing
  | otherwise = Just (VStr picked)
  where
    picked = Text.take 1 (Text.drop i s)

-- | A canonical array index: digits, no leading zero.
arrayIndex :: Text -> Maybe Int
arrayIndex key
  | Text.null key || Text.length key > 9 = Nothing
  | not (Text.all isDigit key) = Nothing
  | Text.length key > 1 && "0" `Text.isPrefixOf` key = Nothing
  | otherwise = Just (Text.foldl' (\acc c -> acc * 10 + digitToInt c) 0 key)

-- | @substr@: JavaScript's @substr@, plus the reference's PHP-style
-- twist that a negative third argument means "stop that many characters
-- before the end" rather than a negative length.
substring :: Text -> Val -> Maybe Val -> Text
substring source start end = case end of
  Just limit | lessThan limit (VNum 0) -> negativeEnd limit
  _ -> substrJs source (toNumber start) (fmap toNumber end)
  where
    tailText = substrJs source (toNumber start) Nothing
    negativeEnd (VNum n) =
      substrJs tailText 0 (Just (fromIntegral (Text.length tailText) + n))
    negativeEnd _ = ""

-- | @String.prototype.substr(start, length)@.
substrJs :: Text -> Double -> Maybe Double -> Text
substrJs source start len = Text.take count (Text.drop from source)
  where
    size = Text.length source
    relative = toClampedInt (negate size, size) start
    from = if relative < 0 then size + relative else relative
    available = size - from
    count = maybe available (toClampedInt (0, available)) len

-- | Truncate toward zero, clamped into the inclusive range; @NaN@ is zero.
toClampedInt :: (Int, Int) -> Double -> Int
toClampedInt (low, high) d
  | isNaN d = max low (min high 0)
  | otherwise = truncate (max (fromIntegral low) (min (fromIntegral high) d))
