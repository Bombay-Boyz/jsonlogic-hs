{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : JsonLogic.Core.Eval
--
-- Evaluation: a catamorphism (a fold) over the rule tree.
--
-- The fold's carrier is a /function/ from the data a rule is evaluated
-- against to a result (@Eval@), not a plain value. That is what lets
-- @map@, @filter@, @reduce@ and friends run the same sub-rule once per
-- element against a different datum, and what makes @if@, @and@ and @or@
-- short-circuit for free: a branch not taken is a function never applied
-- (VISION.md §4, §6).
--
-- Evaluation runs in @Run@, a small state-passing monad whose state is the
-- remaining step budget. It is pure: the budget is threaded through the
-- evaluation, never held in a global or in the 'IO' monad. See
-- "JsonLogic.Core.Config" for the limits.
--
-- Operand evaluation order follows the reference: everything is
-- evaluated before its operator runs, except where the reference
-- evaluates lazily (@if@, @and@, @or@) or against a scoped datum
-- (@map@, @filter@, @reduce@, @all@, @none@, @some@).
module JsonLogic.Core.Eval
  ( CustomOperator
  , Operators
  , OperatorsError (..)
  , evaluate
  , evaluateWith
  , noOperators
  , operatorsFromList
  ) where

import Control.Applicative ((<|>))
import Control.Monad (filterM, foldM, when)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Char (digitToInt, isDigit)
import Data.Functor.Foldable (cata)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

import JsonLogic.Core.Config (Config (..), defaultConfig)
import JsonLogic.Core.Errors (EvalError (..), LimitKind (..), OperatorsError (..))
import JsonLogic.Core.Parse (builtinOperatorNames)
import JsonLogic.Core.Rule (OperatorName (..), Rule, RuleF (..))
import JsonLogic.Core.Utf16 (utf16CharAt, utf16Length, utf16Slice)
import JsonLogic.Core.Value

-- ---------------------------------------------------------------------
-- The evaluation monad
-- ---------------------------------------------------------------------

-- | The result of a step, with the budget left. Strict, so the budget
-- cannot pile up as a chain of thunks.
data Result a = Result !a {-# UNPACK #-} !Int

-- | A computation that may fail with an 'EvalError' and spends steps from a
-- budget.
newtype Run a = Run {unRun :: Int -> Either EvalError (Result a)}

instance Functor Run where
  fmap f (Run m) = Run $ \budget -> case m budget of
    Left e -> Left e
    Right (Result a left) -> Right (Result (f a) left)

instance Applicative Run where
  pure a = Run $ \budget -> Right (Result a budget)
  Run mf <*> Run ma = Run $ \budget -> case mf budget of
    Left e -> Left e
    Right (Result f left) -> case ma left of
      Left e -> Left e
      Right (Result a left') -> Right (Result (f a) left')

instance Monad Run where
  Run m >>= k = Run $ \budget -> case m budget of
    Left e -> Left e
    Right (Result a left) -> unRun (k a) left

throwRun :: EvalError -> Run a
throwRun e = Run (const (Left e))

-- | Spend @n@ steps, or fail if fewer than @n@ remain.
charge :: Config -> Int -> Run ()
charge config n = Run $ \budget ->
  if n > budget
    then Left (LimitExceeded (StepLimit (maxSteps config)))
    else Right (Result () (budget - n))

-- | Spend steps for converting an array operand, which costs as many steps
-- as the array has elements. Everything else converts for free.
chargeConversion :: Config -> Val -> Run ()
chargeConversion config value = case value of
  VArr _ -> Run $ \budget ->
    let weight = valWeightUpTo (budget + 1) value
     in if weight > budget
          then Left (LimitExceeded (StepLimit (maxSteps config)))
          else Right (Result () (budget - weight))
  _ -> pure ()

-- | A rule compiled to a function of the data it is evaluated against.
type Eval = Val -> Run Val

-- | A caller-supplied operator: given the data being evaluated against
-- and its already-evaluated arguments, produce a result or a reason.
--
-- The contract, in full:
--
-- * The operator is looked up by name after all of its operands have been
--   evaluated, left to right, with no short-circuiting (as for the
--   reference's own ordinary operators). An unknown operator is therefore
--   reported only after its operands have run.
-- * The data argument is the datum in scope: inside @map@, @filter@, and
--   so on, the current element. It is converted to JSON lazily, so an
--   operator that ignores it pays nothing for it.
-- * Arguments and result are JSON. JavaScript values with no JSON form
--   cross the boundary as @null@: @NaN@, the infinities, and @undefined@.
-- * The number of arguments is not checked by the library; the operator
--   checks its own.
-- * @Left reason@ stops the evaluation with 'CustomOperatorFailed'.
-- * The result is held to the configured depth and step limits.
type CustomOperator = Aeson.Value -> [Aeson.Value] -> Either Text Aeson.Value

-- | The operators available beyond the builtin set (Standard 0.5: a
-- Handle, supplied at the one place that needs it).
newtype Operators = Operators {lookupOperator :: OperatorName -> Maybe CustomOperator}

-- | Registries compose left-biased: where both define a name, the left one
-- wins.
instance Semigroup Operators where
  Operators f <> Operators g = Operators (\name -> f name <|> g name)

instance Monoid Operators where
  mempty = noOperators

-- | No custom operators: any operator outside the builtin set is an error.
noOperators :: Operators
noOperators = Operators (const Nothing)

-- | Custom operators from an association list. A name that is a builtin
-- operator, or that appears twice, is an error rather than being silently
-- ignored.
operatorsFromList :: [(OperatorName, CustomOperator)] -> Either OperatorsError Operators
operatorsFromList entries = do
  table <- foldM add Map.empty entries
  Right (Operators (`Map.lookup` table))
  where
    add table (name@(OperatorName text), implementation)
      | text `elem` builtinOperatorNames = Left (ReservedOperatorName name)
      | Map.member name table = Left (DuplicateOperatorName name)
      | otherwise = Right (Map.insert name implementation table)

-- | Evaluate a rule against some data, with the default limits and no
-- custom operators.
evaluate :: Rule -> Aeson.Value -> Either EvalError Aeson.Value
evaluate = evaluateWith defaultConfig noOperators

-- | Evaluate a rule against some data.
evaluateWith :: Config -> Operators -> Rule -> Aeson.Value -> Either EvalError Aeson.Value
evaluateWith config operators rule input =
  case unRun (cata (algebra config operators) rule (fromValue input)) (maxSteps config) of
    Left e -> Left e
    Right (Result result _) -> Right (toValue result)

-- | One layer of the fold: given each child already compiled to an
-- @Eval@, compile this node. Every node evaluated costs one step.
algebra :: Config -> Operators -> RuleF Eval -> Eval
algebra config operators node = \env -> charge config 1 >> body env
  where
    body = compile config operators node

compile :: Config -> Operators -> RuleF Eval -> Eval
compile config operators node = case node of
  Lit literal ->
    -- converted once per evaluation, not on every use of the literal: a
    -- literal inside @map@ is shared by all of its iterations
    let converted = fromValue literal in const (pure converted)
  Arr items -> fmap VArr . evalAll items
  Var path fallback -> \env -> do
    key <- path env
    notFound <- maybe (pure VNull) ($ env) fallback
    let (work, found) = lookupVariable (workCap config) env key notFound
    charge config work
    pure found
  Missing keys -> \env -> do
    names <- evalAll keys env
    mapM_ (chargeConversion config) names
    let (work, absent) = missingKeys (workCap config) names env
    charge config work
    pure (VArr absent)
  MissingSome needed options -> \env -> do
    count <- needed env
    listed <- options env
    missingSome config count listed env
  If branches -> evalIf branches
  And operands -> evalAnd operands
  Or operands -> evalOr operands
  Not operand -> fmap (VBool . not . truthy) . operand
  Cast operand -> fmap (VBool . truthy) . operand
  LooseEq x y -> coercing config (\a b -> VBool (looseEquals a b)) x y
  StrictEq x y -> binary (\a b -> VBool (strictEquals a b)) x y
  LooseNeq x y -> coercing config (\a b -> VBool (not (looseEquals a b))) x y
  StrictNeq x y -> binary (\a b -> VBool (not (strictEquals a b))) x y
  Lt x y z -> between config lessThan x y z
  Lte x y z -> between config lessOrEqual x y z
  Gt x y -> coercing config (\a b -> VBool (lessThan b a)) x y
  Gte x y -> coercing config (\a b -> VBool (lessOrEqual b a)) x y
  Max operands -> fmap (VNum . maximumJs . map toNumber) . numeric config operands
  Min operands -> fmap (VNum . minimumJs . map toNumber) . numeric config operands
  Add operands ->
    fmap (VNum . List.foldl' (\total v -> total + parseFloat v) 0) . numeric config operands
  Sub x y -> \env -> do
    a <- x env
    b <- definedOperand <$> traverse ($ env) y
    chargeConversion config a
    mapM_ (chargeConversion config) b
    pure (VNum (maybe (negate (toNumber a)) (\v -> toNumber a - toNumber v) b))
  Mul operands -> fmap multiply . numericNonEmpty config operands
  Div x y -> coercing config (\a b -> VNum (toNumber a / toNumber b)) x y
  Mod x y -> coercing config (\a b -> VNum (remainderJs (toNumber a) (toNumber b))) x y
  Map collection logic ->
    withCollection collection (VArr []) $ \items -> do
      guardInput config items
      VArr <$> traverse logic items
  Filter collection logic ->
    withCollection collection (VArr []) $ \items -> do
      guardInput config items
      VArr <$> filterM (fmap truthy . logic) items
  Reduce collection logic initial -> reduceRule collection logic initial
  All collection logic ->
    withCollection collection (VBool False) $ \items ->
      if null items then pure (VBool False) else VBool <$> allTruthy logic items
  None collection logic ->
    withCollection collection (VBool True) $ \items ->
      if null items then pure (VBool True) else VBool . not <$> anyTruthy logic items
  Some collection logic ->
    withCollection collection (VBool False) $ \items ->
      if null items then pure (VBool False) else VBool <$> anyTruthy logic items
  Merge operands -> \env -> do
    values <- evalAll operands env
    let result = concatMap flattenOnce values
    spendOnLength config (maxArrayLength config) ArrayLimit (lengthUpTo (maxArrayLength config + 1) result)
    pure (VArr result)
  In needle haystack -> coercing config (\a b -> VBool (contains a b)) needle haystack
  Cat operands -> \env -> do
    values <- evalAll operands env
    mapM_ (chargeConversion config) values
    let pieces = map toJoinText values
    total <- foldM (addLength config) 0 pieces
    charge config total
    pure (VStr (Text.concat pieces))
  Substr source start len -> \env -> do
    text <- source env
    from <- start env
    limit <- definedOperand <$> traverse ($ env) len
    chargeConversion config text
    pure (VStr (substring (toJsString text) from limit))
  Log operand -> operand
  Discard inner extras -> \env -> do
    result <- inner env
    mapM_ ($ env) extras
    pure result
  CustomOp name operands -> \env -> do
    arguments <- evalAll operands env
    case lookupOperator operators name of
      Nothing -> throwRun (UnknownOperator name)
      Just implementation ->
        case implementation (toValue env) (map toValue arguments) of
          Left reason -> throwRun (CustomOperatorFailed name reason)
          Right result -> do
            when (nestsDeeperThan (maxDepth config) result) $
              throwRun (LimitExceeded (ValueDepthLimit (maxDepth config)))
            charge config (jsonCost config result)
            pure (fromValue result)

-- | What a value a custom operator returned costs to take in: one step per
-- node, counted only as far as the whole budget could ever pay.
jsonCost :: Config -> Aeson.Value -> Int
jsonCost config = jsonWeightUpTo (maxSteps config + 1)

-- ---------------------------------------------------------------------
-- Combinators over compiled operands
-- ---------------------------------------------------------------------

evalAll :: (Traversable t) => t Eval -> Val -> Run (t Val)
evalAll operands env = traverse ($ env) operands

-- | Two operands, no conversion cost (the combiner never walks an array).
binary :: (Val -> Val -> Val) -> Eval -> Eval -> Eval
binary combine x y env = combine <$> x env <*> y env

-- | Two operands that the combiner may convert to a string or number,
-- which costs as many steps as an array operand has elements.
coercing :: Config -> (Val -> Val -> Val) -> Eval -> Eval -> Eval
coercing config combine x y env = do
  a <- x env
  b <- y env
  chargeConversion config a
  chargeConversion config b
  pure (combine a b)

-- | Operands about to be converted to numbers, charged for the conversion.
numeric :: Config -> [Eval] -> Val -> Run [Val]
numeric config operands env = do
  values <- evalAll operands env
  mapM_ (chargeConversion config) values
  pure values

numericNonEmpty :: Config -> NonEmpty Eval -> Val -> Run (NonEmpty Val)
numericNonEmpty config operands env = do
  values <- evalAll operands env
  mapM_ (chargeConversion config) values
  pure values

-- | @x op y@, or the chained @x op y && y op z@ when a third operand is
-- given. All operands are evaluated first, as in the reference.
between :: Config -> (Val -> Val -> Bool) -> Eval -> Eval -> Maybe Eval -> Eval
between config holds x y z env = do
  a <- x env
  b <- y env
  c <- definedOperand <$> traverse ($ env) z
  mapM_ (chargeConversion config) [a, b]
  mapM_ (chargeConversion config) c
  pure (VBool (maybe (holds a b) (\third -> holds a b && holds b third) c))

-- | Condition/consequent pairs, then an optional else; no branch means null.
--
-- Hand-written recursion (Standard 4.1): a search that stops at the first
-- condition that holds, in 'Run'. It is a right fold with early exit, and
-- @base@ has no monadic combinator for that shape.
evalIf :: [Eval] -> Eval
evalIf (condition : consequent : rest) env = do
  decided <- condition env
  if truthy decided then consequent env else evalIf rest env
evalIf [fallback] env = fallback env
evalIf [] _ = pure VNull

-- | The first falsy operand, or the last; JavaScript's @undefined@ (not
-- null) when there are none.
--
-- Hand-written recursion (Standard 4.1): a right fold with early exit in
-- 'Run'; @base@ has no monadic combinator for that shape.
evalAnd :: [Eval] -> Eval
evalAnd [] _ = pure VUndefined
evalAnd [only] env = only env
evalAnd (first : rest) env = do
  value <- first env
  if truthy value then evalAnd rest env else pure value

-- | The first truthy operand, or the last; JavaScript's @undefined@ (not
-- null) when there are none.
--
-- Hand-written recursion (Standard 4.1): the dual of 'evalAnd'.
evalOr :: [Eval] -> Eval
evalOr [] _ = pure VUndefined
evalOr [only] env = only env
evalOr (first : rest) env = do
  value <- first env
  if truthy value then pure value else evalOr rest env

-- | Evaluate a collection operand; run @onItems@ if it is an array,
-- otherwise answer @fallback@.
withCollection :: Eval -> Val -> ([Val] -> Run Val) -> Eval
withCollection collection fallback onItems env = do
  evaluated <- collection env
  case evaluated of
    VArr items -> onItems items
    _ -> pure fallback

-- | Refuse to iterate an array longer than the array limit, before doing
-- any of the work.
guardInput :: Config -> [Val] -> Run ()
guardInput config items =
  when (lengthUpTo (maxArrayLength config + 1) items > maxArrayLength config) $
    throwRun (LimitExceeded (ArrayLimit (maxArrayLength config)))

-- | The length of a list, counted only up to @cap@.
lengthUpTo :: Int -> [a] -> Int
lengthUpTo cap = go 0
  where
    go n [] = n
    go n (_ : rest)
      | n >= cap = n
      | otherwise = go (n + 1) rest

-- | Fail if a built result is longer than the limit; otherwise spend its
-- length in steps, so building is paid for in proportion to its size.
spendOnLength :: Config -> Int -> (Int -> LimitKind) -> Int -> Run ()
spendOnLength config limit kind size = do
  when (size > limit) $ throwRun (LimitExceeded (kind limit))
  charge config size

-- | Add a piece's length (in UTF-16 code units) to a running total,
-- failing as soon as the total passes the string limit.
addLength :: Config -> Int -> Text -> Run Int
addLength config total piece
  | total' > maxStringLength config = throwRun (LimitExceeded (StringLimit (maxStringLength config)))
  | otherwise = pure total'
  where
    total' = total + utf16Length piece

-- | Fold the array with the sub-rule, evaluated against an object holding
-- the current element and the accumulator.
reduceRule :: Eval -> Eval -> Eval -> Eval
reduceRule collection logic initial env = do
  evaluated <- collection env
  start <- initial env
  case evaluated of
    VArr items -> foldM step start items
    _ -> pure start
  where
    step accumulator item =
      logic (VObj (KeyMap.fromList [("current", item), ("accumulator", accumulator)]))

-- | Does the sub-rule hold for every item? Stops at the first that does not.
--
-- Hand-written recursion (Standard 4.1): a monadic @all@, which @base@ does
-- not provide, in 'Run'.
allTruthy :: Eval -> [Val] -> Run Bool
allTruthy _ [] = pure True
allTruthy logic (item : rest) = do
  value <- logic item
  if truthy value then allTruthy logic rest else pure False

-- | Does the sub-rule hold for some item? Stops at the first that does.
--
-- Hand-written recursion (Standard 4.1): a monadic @any@, the dual of
-- 'allTruthy'.
anyTruthy :: Eval -> [Val] -> Run Bool
anyTruthy _ [] = pure False
anyTruthy logic (item : rest) = do
  value <- logic item
  if truthy value then pure True else anyTruthy logic rest

-- ---------------------------------------------------------------------
-- Operator semantics
-- ---------------------------------------------------------------------

-- | The reference folds @*@ with no seed: a single operand is returned
-- untouched (not converted to a number), two or more are parsed and
-- multiplied.
multiply :: NonEmpty Val -> Val
multiply (first :| rest) =
  List.foldl' (\acc v -> VNum (parseFloat acc * parseFloat v)) first rest

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
--
-- The first component is the work the lookups did, in steps (see
-- 'lookupVariable'); the cap bounds how far any one lookup is followed.
missingKeys :: Int -> [Val] -> Val -> (Int, [Val])
missingKeys cap args dat =
  ( sum [work | (_, work, _) <- probes]
  , [key | (key, _, True) <- probes]
  )
  where
    keys = case args of
      VArr listed : _ -> listed
      _ -> args
    probes = map probe keys
    probe key =
      let (work, found) = lookupKey key
       in (key, work, isMissing found)
    isMissing found = case found of
      VNull -> True
      VStr s -> Text.null s
      _ -> False
    -- The reference looks each key up with @{"var": key}@, so a key that is
    -- itself an array is a @var@'s argument list: path, then default.
    lookupKey key = case key of
      VArr [] -> (0, dat)
      VArr (path : rest) -> lookupVariable cap dat path (fromMaybe VNull (listToMaybe rest))
      _ -> lookupVariable cap dat key VNull

-- | @missing_some@: nothing is missing if at least @count@ of the options
-- are present; otherwise the missing ones.
--
-- The reference reads @options.length@, so a @null@ or undefined option
-- list throws; a string counts its code units; anything without a length
-- compares as @NaN@, which is never enough, so the missing ones are
-- returned.
missingSome :: Config -> Val -> Val -> Val -> Run Val
missingSome config count options env = case options of
  VNull -> throwRun (InvalidOperand (OperatorName "missing_some"))
  VUndefined -> throwRun (InvalidOperand (OperatorName "missing_some"))
  _ -> do
    chargeConversion config options
    let names = case options of
          VArr items -> items
          other -> [other]
        (work, absent) = missingKeys (workCap config) names env
    charge config work
    let have = case optionLength of
          Just n -> n - fromIntegral (length absent)
          Nothing -> 0 / 0
    pure (if lessOrEqual count (VNum have) then VArr [] else VArr absent)
  where
    optionLength = case options of
      VArr items -> Just (fromIntegral (length items))
      VStr s -> Just (fromIntegral (utf16Length s))
      VObj fields -> toNumber <$> KeyMap.lookup "length" fields
      _ -> Nothing

-- | The most work a lookup is followed for: one more than the whole budget,
-- so a lookup that cannot be paid for is cut short rather than completed.
workCap :: Config -> Int
workCap config = maxSteps config + 1

-- | @var@: look a dotted path up in the data. An empty or null path is
-- the data itself; a missing step gives the fallback.
--
-- The first component is the work the walk did, in steps: one per array
-- element passed over and one per code unit of a string indexed or measured
-- (see 'property'). The caller charges it, so a lookup into a large array
-- is paid for in proportion to its size. It is counted only up to @cap@.
lookupVariable :: Int -> Val -> Val -> Val -> (Int, Val)
lookupVariable cap dat path notFound
  | isEmptyPath path = (0, dat)
  | otherwise = (work, fromMaybe (nullIfUndefined notFound) found)
  where
    (work, found) = descend cap dat (Text.splitOn "." (toJsString path))
    isEmptyPath VNull = True
    isEmptyPath VUndefined = True
    isEmptyPath (VStr s) = Text.null s
    isEmptyPath _ = False

-- | An optional operand that evaluates to undefined counts as absent: the
-- reference tests the /value/ (@c === undefined@), not how many
-- arguments were written. So @{"-": [x, y]}@ negates @x@ when @y@ is
-- undefined, @<@ and @<=@ drop an undefined third operand, and @substr@
-- ignores an undefined end.
definedOperand :: Maybe Val -> Maybe Val
definedOperand (Just VUndefined) = Nothing
definedOperand other = other

-- | The reference's fallback for a missing variable: an undefined
-- fallback becomes null.
nullIfUndefined :: Val -> Val
nullIfUndefined VUndefined = VNull
nullIfUndefined value = value

-- | Follow the path one key at a time; 'Nothing' is "no such property".
-- The first component is the work done, summed over the keys and given up
-- as soon as it passes @cap@ (the caller then fails the evaluation).
descend :: Int -> Val -> [Text] -> (Int, Maybe Val)
descend cap = go 0
  where
    go spent current keys
      | spent > cap = (spent, Nothing)
      | otherwise = case keys of
          [] -> (spent, Just current)
          key : rest -> case current of
            VNull -> (spent, Nothing)
            VUndefined -> (spent, Nothing)
            _ ->
              let (work, found) = property cap current key
                  spent' = spent + work
               in case found of
                    Nothing -> (spent', Nothing)
                    -- a property that is itself undefined counts as missing,
                    -- as in the reference
                    Just VUndefined -> (spent', Nothing)
                    Just child -> go spent' child rest

-- | JavaScript property access on a JSON value: object keys, array and
-- string indices, and @length@. Only a value's own properties are seen:
-- the reference would also find names inherited through the prototype
-- chain (@constructor@, @toString@, ...), which JSON data does not have
-- (docs/COMPATIBILITY.md, DEV-INHERITED).
--
-- The first component is the work, in steps: an object lookup is free, an
-- array index costs the elements before it (counted up to @cap@), measuring
-- an array costs its length, and a string costs its length in code points.
property :: Int -> Val -> Text -> (Int, Maybe Val)
property _ (VObj fields) key = (0, KeyMap.lookup (Key.fromText key) fields)
property cap (VArr items) key
  | key == "length" = (lengthUpTo cap items, Just (VNum (fromIntegral (length items))))
  | otherwise = case arrayIndex key of
      Nothing -> (0, Nothing)
      Just i -> (lengthUpTo (min i cap) items, listToMaybe (drop i items))
property cap (VStr s) key
  | key == "length" = (size, Just (VNum (fromIntegral (utf16Length s))))
  | otherwise = case arrayIndex key of
      Nothing -> (0, Nothing)
      Just i -> (size, VStr <$> utf16CharAt i s)
  where
    size = min cap (Text.length s)
property _ _ _ = (0, Nothing)

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
      substrJs tailText 0 (Just (fromIntegral (utf16Length tailText) + n))
    negativeEnd _ = ""

-- | @String.prototype.substr(start, length)@, in UTF-16 code units.
substrJs :: Text -> Double -> Maybe Double -> Text
substrJs source start len = utf16Slice from count source
  where
    size = utf16Length source
    relative = toClampedInt (negate size, size) start
    from = if relative < 0 then size + relative else relative
    available = size - from
    count = maybe available (toClampedInt (0, available)) len

-- | Truncate toward zero, clamped into the inclusive range; @NaN@ is zero.
toClampedInt :: (Int, Int) -> Double -> Int
toClampedInt (low, high) d
  | isNaN d = max low (min high 0)
  | otherwise = truncate (max (fromIntegral low) (min (fromIntegral high) d))
