{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : JsonLogic.Core.Value
--
-- The value domain rules are evaluated in, and the JavaScript semantics
-- the reference implementation inherits for it.
--
-- JsonLogic is defined by its JavaScript reference implementation, so
-- its behaviour on mixed types (@"1" == 1@, @[] == 0@, @"10" < "9"@) is
-- JavaScript's, not something to be derived from first principles. Per
-- VISION.md §4 this module is deliberately plain: one explicit clause
-- per case, no abstraction layered over an arbitrary table.
--
-- 'Val' is JSON plus what JSON cannot say but JavaScript numbers can:
-- @NaN@ and the infinities. They exist only during evaluation;
-- 'toValue' turns them into JSON @null@, as @JSON.stringify@ does.
--
-- Recorded deviations (VISION.md §11): arrays and objects are never
-- equal to one another under @==@ / @===@ (the reference compares by
-- reference identity, which JSON values do not have); strings are
-- ordered by code point, not UTF-16 code unit.
module JsonLogic.Core.Value
  ( Val (..)
  , fromValue
  , toValue
  , truthy
  , strictEquals
  , looseEquals
  , lessThan
  , lessOrEqual
  , toNumber
  , parseFloat
  , toJsString
  , toJoinText
  , numberToString
  , remainderJs
  , maximumJs
  , minimumJs
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Char (digitToInt, isDigit, isHexDigit, isSpace)
import Data.Foldable (toList)
import Data.List (dropWhileEnd, stripPrefix)
import Data.Scientific (Scientific, fromFloatDigits, scientific, toBoundedRealFloat)
import Data.Text (Text)
import qualified Data.Text as Text
import Numeric (floatToDigits)

-- | A value during evaluation.
data Val
  = VNull
  | -- | JavaScript's @undefined@, which an empty @and@ or @or@ yields. It
    -- is not 'VNull': it converts to @NaN@ (not 0) and to the string
    -- @"undefined"@ (not @"null"@), and @undefined === null@ is false.
    -- Written back as JSON @null@. The differential test found the
    -- difference by folding the two together.
    VUndefined
  | VBool Bool
  | VNum Double
  | VStr Text
  | VArr [Val]
  | VObj (KeyMap.KeyMap Val)
  deriving (Eq, Show)

nan :: Double
nan = 0 / 0

infinity :: Double
infinity = 1 / 0

-- ---------------------------------------------------------------------
-- Conversion to and from JSON
-- ---------------------------------------------------------------------

-- | Read JSON into the evaluation domain. Numbers become doubles, as in
-- the reference; out-of-range exponents saturate instead of hanging.
fromValue :: Aeson.Value -> Val
fromValue Aeson.Null = VNull
fromValue (Aeson.Bool flag) = VBool flag
fromValue (Aeson.Number n) = VNum (scientificToDouble n)
fromValue (Aeson.String s) = VStr s
fromValue (Aeson.Array items) = VArr (map fromValue (toList items))
fromValue (Aeson.Object fields) = VObj (KeyMap.map fromValue fields)

-- | Write a result back as JSON. @NaN@ and infinities become @null@.
toValue :: Val -> Aeson.Value
toValue VNull = Aeson.Null
toValue VUndefined = Aeson.Null
toValue (VBool flag) = Aeson.Bool flag
toValue (VNum d)
  | isNaN d || isInfinite d = Aeson.Null
  | otherwise = Aeson.Number (fromFloatDigits d)
toValue (VStr s) = Aeson.String s
toValue (VArr items) = Aeson.toJSON (map toValue items)
toValue (VObj fields) = Aeson.Object (KeyMap.map toValue fields)

scientificToDouble :: Scientific -> Double
scientificToDouble = either id id . toBoundedRealFloat

-- ---------------------------------------------------------------------
-- Truthiness
-- ---------------------------------------------------------------------

-- | JsonLogic's truthiness: JavaScript's, except that an empty array is
-- falsy (jsonlogic.com/truthy).
truthy :: Val -> Bool
truthy VNull = False
truthy VUndefined = False
truthy (VBool flag) = flag
truthy (VNum d) = not (d == 0 || isNaN d)
truthy (VStr s) = not (Text.null s)
truthy (VArr items) = not (null items)
truthy (VObj _) = True

-- ---------------------------------------------------------------------
-- Conversion to string
-- ---------------------------------------------------------------------

-- | JavaScript's @String(value)@.
toJsString :: Val -> Text
toJsString VNull = "null"
toJsString VUndefined = "undefined"
toJsString (VBool True) = "true"
toJsString (VBool False) = "false"
toJsString (VNum d) = numberToString d
toJsString (VStr s) = s
toJsString (VArr items) = Text.intercalate "," (map toJoinText items)
toJsString (VObj _) = "[object Object]"

-- | How @Array.prototype.join@ renders an element: like 'toJsString',
-- except @null@ becomes the empty string.
toJoinText :: Val -> Text
toJoinText VNull = ""
toJoinText VUndefined = ""
toJoinText value = toJsString value

-- | JavaScript's @Number::toString@ (ECMAScript, radix 10): the shortest
-- digits that round-trip, laid out in plain or exponent form.
numberToString :: Double -> Text
numberToString d
  | isNaN d = "NaN"
  | d == 0 = "0"
  | isInfinite d = if d > 0 then "Infinity" else "-Infinity"
  | d < 0 = "-" <> numberToString (negate d)
  | otherwise = Text.pack (layoutDigits (concatMap show digits) (length digits) pointPosition)
  where
    (digits, pointPosition) = floatToDigits 10 d

-- | @layoutDigits ds k n@: @k@ significant digits @ds@ whose decimal
-- point sits @n@ places to the right of their start.
layoutDigits :: String -> Int -> Int -> String
layoutDigits ds k n
  | k <= n && n <= 21 = ds ++ replicate (n - k) '0'
  | 0 < n && n <= 21 = take n ds ++ "." ++ drop n ds
  | -6 < n && n <= 0 = "0." ++ replicate (negate n) '0' ++ ds
  | otherwise = exponentForm
  where
    power = n - 1
    sign = if power < 0 then "-" else "+"
    exponentForm = case ds of
      [] -> ""
      [c] -> c : "e" ++ sign ++ show (abs power)
      c : cs -> c : '.' : cs ++ "e" ++ sign ++ show (abs power)

-- ---------------------------------------------------------------------
-- Conversion to number
-- ---------------------------------------------------------------------

-- | JavaScript's @Number(value)@ (the abstract @ToNumber@).
toNumber :: Val -> Double
toNumber VNull = 0
toNumber VUndefined = nan
toNumber (VBool flag) = boolToNumber flag
toNumber (VNum d) = d
toNumber (VStr s) = stringToNumber s
toNumber value@(VArr _) = stringToNumber (toJsString value)
toNumber (VObj _) = nan

boolToNumber :: Bool -> Double
boolToNumber flag = if flag then 1 else 0

-- | JavaScript's @parseFloat(String(value))@: the longest decimal prefix,
-- ignoring leading whitespace; @NaN@ if there is none. Unlike 'toNumber'
-- it accepts trailing junk and does not read hexadecimal.
parseFloat :: Val -> Double
parseFloat (VNum d)
  | d == 0 = 0
  | otherwise = d
parseFloat value =
  case decimalPrefix (dropWhile isSpace (Text.unpack (toJsString value))) of
    Just (number, _) -> number
    Nothing -> nan

-- | The string-to-number conversion of @Number(string)@: the whole
-- trimmed string must be a literal; the empty string is zero.
stringToNumber :: Text -> Double
stringToNumber source
  | null trimmed = 0
  | otherwise = case radixLiteral trimmed of
      Just number -> number
      Nothing -> case decimalPrefix trimmed of
        Just (number, []) -> number
        _ -> nan
  where
    trimmed = dropWhileEnd isSpace (dropWhile isSpace (Text.unpack source))

-- | @0x..@, @0o..@, @0b..@ literals (unsigned, at least one digit).
radixLiteral :: String -> Maybe Double
radixLiteral ('0' : marker : digits)
  | not (null digits)
  , Just base <- radixOf marker
  , all (isDigitIn base) digits =
      Just (fromInteger (foldl (\acc c -> acc * base + toInteger (digitToInt c)) 0 digits))
radixLiteral _ = Nothing

radixOf :: Char -> Maybe Integer
radixOf 'x' = Just 16
radixOf 'X' = Just 16
radixOf 'o' = Just 8
radixOf 'O' = Just 8
radixOf 'b' = Just 2
radixOf 'B' = Just 2
radixOf _ = Nothing

isDigitIn :: Integer -> Char -> Bool
isDigitIn base c = isHexDigit c && toInteger (digitToInt c) < base

-- | The longest prefix of the input that is a decimal literal
-- (@[+-]digits[.digits][e[+-]digits]@, @.digits@, or @[+-]Infinity@),
-- with its value and the unread remainder.
decimalPrefix :: String -> Maybe (Double, String)
decimalPrefix input =
  case stripPrefix "Infinity" afterSign of
    Just remainder -> Just (applySign infinity, remainder)
    Nothing -> fmap (\(magnitude, remainder) -> (applySign magnitude, remainder)) (unsignedDecimal afterSign)
  where
    (negative, afterSign) = case input of
      '-' : rest -> (True, rest)
      '+' : rest -> (False, rest)
      _ -> (False, input)
    applySign x = if negative then negate x else x

unsignedDecimal :: String -> Maybe (Double, String)
unsignedDecimal s
  | null intDigits && null fracDigits = Nothing
  | otherwise = Just (assemble intDigits fracDigits exponentValue, remainder)
  where
    (intDigits, afterInt) = span isDigit s
    (fracDigits, afterFrac) = case afterInt of
      '.' : rest -> span isDigit rest
      _ -> ([], afterInt)
    (exponentValue, remainder) = exponentPart afterFrac

-- | An optional exponent. If the marker is not followed by digits it is
-- not part of the literal and is left unread.
exponentPart :: String -> (Integer, String)
exponentPart input = case input of
  marker : rest
    | marker == 'e' || marker == 'E' ->
        let (isNegative, afterSign) = case rest of
              '-' : more -> (True, more)
              '+' : more -> (False, more)
              _ -> (False, rest)
            (digits, remainder) = span isDigit afterSign
         in if null digits
              then (0, input)
              else (withSign isNegative (digitsToInteger digits), remainder)
  _ -> (0, input)
  where
    withSign flag n = if flag then negate n else n

digitsToInteger :: String -> Integer
digitsToInteger = foldl (\acc c -> acc * 10 + toInteger (digitToInt c)) 0

-- | Build the double from decimal digits and a power of ten. The
-- exponent is clamped first: anything beyond it overflows or underflows
-- regardless, and 'scientificToDouble' saturates rather than hanging.
assemble :: String -> String -> Integer -> Double
assemble intDigits fracDigits exponentValue =
  scientificToDouble (scientific coefficient (fromInteger clamped))
  where
    coefficient = digitsToInteger (intDigits ++ fracDigits)
    shifted = exponentValue - toInteger (length fracDigits)
    clamped = max (-100000) (min 100000 shifted)

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
strictEquals _ _ = False

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
looseEquals (VBool a) y = looseEquals (VNum (boolToNumber a)) y
looseEquals x (VBool b) = looseEquals x (VNum (boolToNumber b))
looseEquals (VNum a) (VNum b) = a == b
looseEquals (VStr a) (VStr b) = a == b
looseEquals (VNum a) (VStr b) = a == stringToNumber b
looseEquals (VStr a) (VNum b) = stringToNumber a == b
looseEquals (VArr _) (VArr _) = False
looseEquals (VArr _) (VObj _) = False
looseEquals (VObj _) (VArr _) = False
looseEquals (VObj _) (VObj _) = False
looseEquals x y@(VArr _) = looseEquals x (VStr (toJsString y))
looseEquals x y@(VObj _) = looseEquals x (VStr (toJsString y))
looseEquals x@(VArr _) y = looseEquals (VStr (toJsString x)) y
looseEquals x@(VObj _) y = looseEquals (VStr (toJsString x)) y

-- ---------------------------------------------------------------------
-- Ordering
-- ---------------------------------------------------------------------

-- | Arrays and objects compare as the strings they convert to.
toPrimitive :: Val -> Val
toPrimitive value@(VArr _) = VStr (toJsString value)
toPrimitive value@(VObj _) = VStr (toJsString value)
toPrimitive value = value

-- | JavaScript's @<@: two strings compare lexicographically, anything
-- else numerically (so any @NaN@ makes it false).
lessThan :: Val -> Val -> Bool
lessThan x y = case (toPrimitive x, toPrimitive y) of
  (VStr a, VStr b) -> a < b
  (a, b) -> toNumber a < toNumber b

-- | JavaScript's @<=@.
lessOrEqual :: Val -> Val -> Bool
lessOrEqual x y = case (toPrimitive x, toPrimitive y) of
  (VStr a, VStr b) -> a <= b
  (a, b) -> toNumber a <= toNumber b

-- ---------------------------------------------------------------------
-- Arithmetic helpers
-- ---------------------------------------------------------------------

-- | JavaScript's @%@ on doubles: the remainder takes the dividend's
-- sign. Computed exactly in 'Rational'.
remainderJs :: Double -> Double -> Double
remainderJs x y
  | isNaN x || isNaN y || isInfinite x || y == 0 = nan
  | isInfinite y = x
  | x == 0 = x
  | otherwise = fromRational (rx - ry * fromInteger (truncate (rx / ry)))
  where
    rx = toRational x
    ry = toRational y

-- | @Math.max@: @NaN@ if any argument is, @-Infinity@ for none.
maximumJs :: [Double] -> Double
maximumJs xs
  | any isNaN xs = nan
  | otherwise = foldl max (negate infinity) xs

-- | @Math.min@: @NaN@ if any argument is, @Infinity@ for none.
minimumJs :: [Double] -> Double
minimumJs xs
  | any isNaN xs = nan
  | otherwise = foldl min infinity xs
