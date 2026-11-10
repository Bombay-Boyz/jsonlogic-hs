{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module      : JsonLogic.Core.Value
--
-- The values rules are evaluated over, and the JavaScript semantics the
-- reference implementation gives them.
--
-- JsonLogic is defined by its JavaScript reference implementation, so its
-- behaviour on mixed types (@"1" == 1@, @[] == 0@, @"10" < "9"@) is
-- JavaScript's, not something derived from first principles. This module is
-- deliberately plain: one explicit clause per case of the JavaScript
-- algorithms, no abstraction layered over the table.
--
-- 'Val' is JSON plus what JSON cannot say but a JavaScript evaluation can
-- produce: @NaN@, the infinities, negative zero and @undefined@. They exist
-- only during evaluation; 'toValue' writes them as @JSON.stringify@ does.
--
-- Recorded deviations (docs/COMPATIBILITY.md): arrays and objects are never
-- equal to one another under @==@ / @===@ (the reference compares them by
-- identity, which JSON values do not have). Strings are measured and ordered
-- by UTF-16 code unit, as in JavaScript (see "JsonLogic.Core.Utf16"). A
-- string that holds a lone surrogate (only a slice through a surrogate pair
-- makes one) is a 'VWtf', so that it behaves as in JavaScript: its halves
-- are unequal and re-join into the original character. It becomes U+FFFD
-- only where JSON cannot carry it, in the result.
module JsonLogic.Core.Value
  ( Val (..)
  , numberOf
  , JsStr (..)
  , fromValue
  , fromValueWith
  , firstLossyNumber
  , toValue
  , toJsStr
  , jsStrVal
  , jsStrUnits
  , joinJsStr
  , truthy
  , strictEquals
  , looseEquals
  , lessThan
  , lessOrEqual
  , toNumber
  , parseFloat
  , toJsString
  , toJoinText
  , nestsDeeperThan
  , jsonWeightUpTo
  , valWeightUpTo
  , saturatingSucc
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Foldable (toList)
import Data.List (intersperse)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector

import JsonLogic.Core.Config (NumericSafety (..))
import JsonLogic.Core.Number
  ( doubleToScientific
  , numberToString
  , parseFloatString
  , printsAsSource
  , scientificToDouble
  , stringToNumber
  )
import JsonLogic.Core.Utf16 (Units, compareUtf16, fromUnits, toUnits, unitsAppend, unitsText)

-- | A value during evaluation.
-- The constructors are in this order on purpose. GHC tags a pointer with
-- the constructor only for the first six of a type with more than seven, and
-- finds the others by reading the object, so the six that most evaluations
-- inspect come first and the rare ones last.
data Val
  = VNull
  | VBool !Bool
  | -- | Any JavaScript number, including @NaN@, the infinities and @-0@.
    VNum {-# UNPACK #-} !Double
  | VStr !Text
  | VArr !(Vector Val)
  | VObj !(KeyMap.KeyMap Val)
  | -- | JavaScript's @undefined@, which an empty @and@ or @or@ yields. It
    -- is not 'VNull': it converts to @NaN@ (not 0) and to the string
    -- @\"undefined\"@ (not @\"null\"@), and @undefined === null@ is false.
    -- Written back as JSON @null@, and left out of objects.
    VUndefined
  | -- | A string with a lone surrogate, as its UTF-16 code units. Never
    -- well formed, so never equal to a 'VStr'; built only by 'fromUnits'
    -- through 'jsStrVal'.
    VWtf !Units
  | -- | A JSON number that a double cannot carry, when the configuration
    -- preserves such numbers: the double it is read as, and its own digits.
    -- It behaves as that double everywhere, and is written back as its own
    -- digits if it reaches the result unchanged. Arithmetic builds 'VNum',
    -- so the digits do not survive it. Kept apart from 'VNum' so that
    -- ordinary numbers carry nothing extra.
    VBig {-# UNPACK #-} !Double !Scientific
  deriving (Show)

-- | The double a number value stands for.
numberOf :: Val -> Maybe Double
numberOf (VNum d) = Just d
numberOf (VBig d _) = Just d
numberOf _ = Nothing


-- | Structural equality, in which @NaN@ equals @NaN@, so that equality is
-- reflexive. (This is not JavaScript's @==@, which is 'looseEquals', nor its
-- @===@, which is 'strictEquals'; it compares evaluation results.)
instance Eq Val where
  VNull == VNull = True
  VUndefined == VUndefined = True
  VBool a == VBool b = a == b
  VNum a == VNum b = a == b || (isNaN a && isNaN b)
  a@(VBig _ _) == b | Just x <- numberOf a, Just y <- numberOf b = x == y || (isNaN x && isNaN y)
  a == b@(VBig _ _) | Just x <- numberOf a, Just y <- numberOf b = x == y || (isNaN x && isNaN y)
  VStr a == VStr b = a == b
  VWtf a == VWtf b = a == b
  VArr a == VArr b = a == b
  VObj a == VObj b = a == b
  _ == _ = False

-- ---------------------------------------------------------------------
-- Conversion to and from JSON
-- ---------------------------------------------------------------------

-- | Read JSON into the evaluation domain. Numbers become doubles, as in
-- the reference. The elements of arrays and objects are converted only when
-- they are used.
fromValue :: Aeson.Value -> Val
fromValue Aeson.Null = VNull
fromValue (Aeson.Bool flag) = VBool flag
fromValue (Aeson.Number n) = VNum (scientificToDouble n)
fromValue (Aeson.String s) = VStr s
fromValue (Aeson.Array items) = VArr (Vector.map fromValue items)
fromValue (Aeson.Object fields) = VObj (KeyMap.map fromValue fields)

-- | 'fromValue' under a numeric policy: with 'PreserveSource' (and
-- 'RejectLossy', whose input has already been screened) a number whose
-- double does not print as its digits keeps them. The default policy is
-- plain 'fromValue', which the optimiser fuses into its consumers; the
-- others pay for the exactness check.
fromValueWith :: NumericSafety -> Aeson.Value -> Val
fromValueWith JsNumbers = fromValue
fromValueWith _ = go
  where
    go Aeson.Null = VNull
    go (Aeson.Bool flag) = VBool flag
    go (Aeson.Number n)
      | printsAsSource n = VNum (scientificToDouble n)
      | otherwise = VBig (scientificToDouble n) n
    go (Aeson.String s) = VStr s
    go (Aeson.Array items) = VArr (Vector.map go items)
    go (Aeson.Object fields) = VObj (KeyMap.map go fields)

-- | The first number in a JSON value, in document order, that a double
-- cannot carry (see 'printsAsSource'). Walks the whole value.
firstLossyNumber :: Aeson.Value -> Maybe Scientific
firstLossyNumber value = case value of
  Aeson.Number n | not (printsAsSource n) -> Just n
  Aeson.Array items -> firstJust (Vector.toList items)
  Aeson.Object fields -> firstJust (KeyMap.elems fields)
  _ -> Nothing
  where
    firstJust [] = Nothing
    firstJust (x : xs) = case firstLossyNumber x of
      Nothing -> firstJust xs
      found -> found

-- | Write a result back as JSON, as @JSON.stringify@ would: @NaN@, the
-- infinities and @undefined@ become @null@, negative zero becomes @0@, and an
-- object property whose value is @undefined@ is left out.
toValue :: Val -> Aeson.Value
toValue VNull = Aeson.Null
toValue VUndefined = Aeson.Null
toValue (VBool flag) = Aeson.Bool flag
toValue (VBig _ n) = Aeson.Number n
toValue (VNum d)
  | isNaN d || isInfinite d = Aeson.Null
  | otherwise = Aeson.Number (doubleToScientific d)
toValue (VStr s) = Aeson.String s
toValue (VWtf units) = Aeson.String (unitsText units)
toValue (VArr items) = Aeson.Array (Vector.map toValue items)
toValue (VObj fields) = Aeson.Object (KeyMap.map toValue (KeyMap.filter (not . isUndefined) fields))
  where
    isUndefined VUndefined = True
    isUndefined _ = False

-- | One more, except at 'maxBound', where it stays: a limit of 'maxBound'
-- means "no limit", and a bound written as @limit + 1@ must not wrap round
-- to 'minBound' and refuse everything.
saturatingSucc :: Int -> Int
saturatingSucc n = if n == maxBound then n else n + 1

-- | Does a JSON value nest more than @n@ levels? An array or object that is
-- not empty is one level; a scalar is none. Stops as soon as it knows, so
-- the cost is bounded by @n@ along any path.
nestsDeeperThan :: Int -> Aeson.Value -> Bool
nestsDeeperThan n value = case value of
  Aeson.Array items -> not (null items) && (n <= 0 || any (nestsDeeperThan (n - 1)) items)
  Aeson.Object fields -> not (KeyMap.null fields) && (n <= 0 || any (nestsDeeperThan (n - 1)) (KeyMap.elems fields))
  _ -> False

-- | How many nodes a JSON value has, counted only up to @cap@: the work to
-- take it in, without walking an enormous value to the end.
jsonWeightUpTo :: Int -> Aeson.Value -> Int
jsonWeightUpTo cap value = cap - remaining cap value
  where
    remaining budget v
      | budget <= 0 = 0
      | otherwise = case v of
          Aeson.Array items -> children (budget - 1) (Vector.toList items)
          Aeson.Object fields -> children (budget - 1) (KeyMap.elems fields)
          _ -> budget - 1
    children budget [] = budget
    children budget (x : xs)
      | budget <= 0 = 0
      | otherwise = children (remaining budget x) xs

-- | How many array elements, at any depth, a value holds, counted only up
-- to @cap@. This is the work converting an array to a string or number
-- takes; every other value is free.
valWeightUpTo :: Int -> Val -> Int
valWeightUpTo cap value = cap - remaining cap value
  where
    remaining budget v = case v of
      VArr items -> children budget (Vector.toList items)
      _ -> budget
    children budget [] = budget
    children budget (x : xs)
      | budget <= 0 = 0
      | otherwise = children (remaining (budget - 1) x) xs

-- ---------------------------------------------------------------------
-- Truthiness
-- ---------------------------------------------------------------------

-- | JsonLogic's truthiness: JavaScript's, except that an empty array is
-- falsy (<https://jsonlogic.com/truthy.html>).
truthy :: Val -> Bool
truthy VNull = False
truthy VUndefined = False
truthy (VBool flag) = flag
truthy (VBig d _) = truthy (VNum d)
truthy (VNum d) = not (d == 0 || isNaN d)
truthy (VStr s) = not (Text.null s)
truthy (VWtf _) = True
truthy (VArr items) = not (Vector.null items)
truthy (VObj _) = True

-- ---------------------------------------------------------------------
-- Conversion to string and number
-- ---------------------------------------------------------------------

-- | JavaScript's @String(value)@.
toJsString :: Val -> Text
toJsString VNull = "null"
toJsString VUndefined = "undefined"
toJsString (VBool True) = "true"
toJsString (VBool False) = "false"
toJsString (VBig d _) = numberToString d
toJsString (VNum d) = numberToString d
toJsString (VStr s) = s
toJsString (VWtf units) = unitsText units
toJsString (VArr items) = Text.intercalate "," (map toJoinText (Vector.toList items))
toJsString (VObj _) = "[object Object]"

-- | How @Array.prototype.join@ renders an element: like 'toJsString',
-- except that @null@ and @undefined@ become the empty string.
toJoinText :: Val -> Text
toJoinText VNull = ""
toJoinText VUndefined = ""
toJoinText value = toJsString value

-- | JavaScript's @Number(value)@ (the abstract @ToNumber@).
toNumber :: Val -> Double
toNumber VNull = 0
toNumber VUndefined = 0 / 0
toNumber (VBool flag) = boolToNumber flag
toNumber (VBig d _) = d
toNumber (VNum d) = d
toNumber (VStr s) = stringToNumber s
toNumber (VWtf units) = stringToNumber (unitsText units)
toNumber value@(VArr _) = stringToNumber (toJsString value)
toNumber (VObj _) = 0 / 0

boolToNumber :: Bool -> Double
boolToNumber flag = if flag then 1 else 0

-- | JavaScript's @parseFloat(String(value))@, which is how @+@ and @*@ read
-- their operands: the longest decimal prefix, ignoring leading whitespace;
-- @NaN@ if there is none. A number is itself, except that negative zero
-- becomes zero (@String(-0)@ is @"0"@).
parseFloat :: Val -> Double
parseFloat (VBig d _) = d
parseFloat (VNum d)
  | d == 0 = 0
  | otherwise = d
parseFloat value = parseFloatString (toJsString value)

-- ---------------------------------------------------------------------
-- Equality
-- ---------------------------------------------------------------------

-- | JavaScript's @===@. Arrays and objects are never equal (see the
-- module header).
strictEquals :: Val -> Val -> Bool
strictEquals VNull VNull = True
strictEquals VUndefined VUndefined = True
strictEquals (VBool a) (VBool b) = a == b
strictEquals (VNum a) (VNum b) = a == b
strictEquals (VStr a) (VStr b) = a == b
strictEquals (VWtf a) (VWtf b) = a == b
strictEquals a b
  | Just x <- numberOf a, Just y <- numberOf b = x == y
  | otherwise = False

-- | JavaScript's @==@ (the abstract equality comparison).
looseEquals :: Val -> Val -> Bool
looseEquals VUndefined VUndefined = True
looseEquals VUndefined VNull = True
looseEquals VNull VUndefined = True
looseEquals VUndefined _ = False
looseEquals _ VUndefined = False
looseEquals VNull VNull = True
looseEquals VNull _ = False
looseEquals _ VNull = False
looseEquals (VBig d _) y = looseEquals (VNum d) y
looseEquals x (VBig d _) = looseEquals x (VNum d)
looseEquals (VBool a) y = looseEquals (VNum (boolToNumber a)) y
looseEquals x (VBool b) = looseEquals x (VNum (boolToNumber b))
looseEquals (VNum a) (VNum b) = a == b
looseEquals (VStr a) (VStr b) = a == b
looseEquals (VWtf a) (VWtf b) = a == b
-- a string with a lone surrogate is never equal to a well-formed one, and
-- read as a number it is NaN
looseEquals (VWtf _) (VStr _) = False
looseEquals (VStr _) (VWtf _) = False
looseEquals (VWtf units) y@(VNum _) = looseEquals (VStr (unitsText units)) y
looseEquals x@(VNum _) (VWtf units) = looseEquals x (VStr (unitsText units))
looseEquals (VNum a) (VStr b) = a == stringToNumber b
looseEquals (VStr a) (VNum b) = stringToNumber a == b
looseEquals (VArr _) (VArr _) = False
looseEquals (VArr _) (VObj _) = False
looseEquals (VObj _) (VArr _) = False
looseEquals (VObj _) (VObj _) = False
looseEquals x y@(VArr _) = looseEquals x (jsStrVal (toJsStr y))
looseEquals x y@(VObj _) = looseEquals x (jsStrVal (toJsStr y))
looseEquals x@(VArr _) y = looseEquals (jsStrVal (toJsStr x)) y
looseEquals x@(VObj _) y = looseEquals (jsStrVal (toJsStr x)) y

-- ---------------------------------------------------------------------
-- Ordering
-- ---------------------------------------------------------------------

-- | Arrays and objects compare as the strings they convert to.
toPrimitive :: Val -> Val
toPrimitive value@(VArr _) = jsStrVal (toJsStr value)
toPrimitive value@(VObj _) = VStr (toJsString value)
toPrimitive value = value

-- | A string value, if it is one.
stringOf :: Val -> Maybe JsStr
stringOf (VStr s) = Just (Plain s)
stringOf (VWtf units) = Just (Wtf units)
stringOf _ = Nothing

-- | JavaScript's @<@: two strings compare by UTF-16 code unit, anything
-- else numerically (so any @NaN@ makes it false).
lessThan :: Val -> Val -> Bool
lessThan x y = case (toPrimitive x, toPrimitive y) of
  (a, b)
    | Just sa <- stringOf a, Just sb <- stringOf b -> compareStr sa sb == LT
    | otherwise -> toNumber a < toNumber b

-- | JavaScript's @<=@.
lessOrEqual :: Val -> Val -> Bool
lessOrEqual x y = case (toPrimitive x, toPrimitive y) of
  (a, b)
    | Just sa <- stringOf a, Just sb <- stringOf b -> compareStr sa sb /= GT
    | otherwise -> toNumber a <= toNumber b

-- ---------------------------------------------------------------------
-- Strings, exactly
-- ---------------------------------------------------------------------

-- | A JavaScript string: a 'Text' when it is well formed, which is nearly
-- always, and its code units when it holds a lone surrogate.
data JsStr
  = Plain !Text
  | Wtf !Units

-- | The code units of a string.
jsStrUnits :: JsStr -> Units
jsStrUnits (Plain t) = toUnits t
jsStrUnits (Wtf u) = u

-- | A string as a value.
jsStrVal :: JsStr -> Val
jsStrVal (Plain t) = VStr t
jsStrVal (Wtf u) = VWtf u

-- | Concatenate strings. Halves of a surrogate pair that meet become the
-- pair, so the result is a 'Plain' again.
joinJsStr :: Text -> [JsStr] -> JsStr
joinJsStr separator parts
  | all isPlain parts = Plain (Text.intercalate separator [t | Plain t <- parts])
  | otherwise = case fromUnits (unitsAppend (intersperse (toUnits separator) (map jsStrUnits parts))) of
      Right t -> Plain t
      Left u -> Wtf u
  where
    isPlain (Plain _) = True
    isPlain _ = False

-- | JavaScript's @String(value)@, exactly: an array holding a lone surrogate
-- converts to a string that holds it.
toJsStr :: Val -> JsStr
toJsStr (VStr s) = Plain s
toJsStr (VWtf u) = Wtf u
toJsStr (VArr items) = joinJsStr "," (map joined (toList items))
  where
    joined VNull = Plain ""
    joined VUndefined = Plain ""
    joined v = toJsStr v
toJsStr v = Plain (toJsString v)

compareStr :: JsStr -> JsStr -> Ordering
compareStr (Plain a) (Plain b) = compareUtf16 a b
compareStr a b = compare (jsStrUnits a) (jsStrUnits b)
