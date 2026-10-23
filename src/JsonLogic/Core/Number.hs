{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : JsonLogic.Core.Number
--
-- JavaScript numbers: reading decimal text into a double, writing a double
-- as the text JavaScript would, and the arithmetic helpers whose JavaScript
-- definitions differ from Haskell's.
--
-- Every conversion is exact: a decimal is rounded to the nearest double
-- once, and a double is written with the shortest digits that read back as
-- it, chosen as ECMAScript chooses them. Reading has a fast path for the
-- common case, taken only when it is provably exact, and an exact general
-- path in 'Rational'; writing uses Ryū. The test suite checks the fast
-- algorithms against slow, exact ones.
module JsonLogic.Core.Number
  ( -- * Reading
    decimalToDouble
  , scientificToDouble
  , stringToNumber
  , parseFloatString
  , isJsSpace

    -- * Writing
  , numberToString
  , doubleToScientific
  , shortestDigits
  , shortestDigitsExact

    -- * Arithmetic
  , remainderJs
  , maximumJs
  , minimumJs
  ) where

import Data.Bifunctor (first)
import Data.Bits (shiftL, shiftR, (.&.))
import Data.Char (digitToInt, isDigit, isHexDigit)
import Data.Int (Int64)
import Data.List (dropWhileEnd, sortOn, stripPrefix)
import qualified Data.List as List
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Scientific (Scientific, base10Exponent, coefficient, scientific)
import Data.Text (Text)
import qualified Data.Text as Text
import Numeric (floatToDigits)

import JsonLogic.Core.Ryu (shortestDecimalRyu)

nan :: Double
nan = 0 / 0

infinity :: Double
infinity = 1 / 0

-- | 2^53: below it every integer is a double, and so is every product or
-- quotient of such an integer and a power of ten up to 10^22 correctly
-- rounded by one IEEE operation.
twoTo53 :: Double
twoTo53 = 9007199254740992

-- | @10^n@ for @0 <= n <= 22@, the powers of ten that are exact doubles.
-- Every intermediate product of '(^)' is a smaller power of ten, so exact too.
powerOfTen :: Integer -> Double
powerOfTen n = 10 ^ n

-- ---------------------------------------------------------------------
-- Reading
-- ---------------------------------------------------------------------

-- | A JSON number as the double JavaScript's @JSON.parse@ would make of it.
scientificToDouble :: Scientific -> Double
scientificToDouble s = decimalToDouble (coefficient s) (toInteger (base10Exponent s))

-- | @mantissa * 10 ^ exponent10@ as the nearest double (ties to even).
--
-- When the mantissa is below 2^53 and the exponent within 22 of zero, both
-- factors are exact doubles and one IEEE multiplication or division rounds
-- the result correctly (Clinger's fast path, \"How to read floating point
-- numbers accurately\", 1990). Otherwise the value is computed exactly in
-- integers and rounded once ('roundRatio'). Magnitudes that must overflow or
-- underflow are decided from the digit count, so a hostile exponent cannot
-- make this build an enormous number.
decimalToDouble :: Integer -> Integer -> Double
decimalToDouble mantissa exponent10
  | mantissa == 0 = 0
  | mantissa < 0 = negate (decimalToDouble (negate mantissa) exponent10)
  | mantissa < 9007199254740992 && 0 <= exponent10 && exponent10 <= 22 =
      fromInteger mantissa * powerOfTen exponent10
  | mantissa < 9007199254740992 && -22 <= exponent10 && exponent10 < 0 =
      fromInteger mantissa / powerOfTen (negate exponent10)
  | magnitude > 310 = infinity
  | magnitude < -330 = 0
  | exponent10 >= 0 = roundRatio (mantissa * 10 ^ exponent10) 1
  | otherwise = roundRatio mantissa (10 ^ negate exponent10)
  where
    magnitude = exponent10 + toInteger (length (show mantissa))

-- | @numerator / denominator@, both positive, as the nearest double, ties to
-- even, including the subnormal range and overflow to infinity.
--
-- The quotient is taken with at least 56 significant bits; the bits below the
-- 53 a double keeps (fewer for a subnormal), together with whether the
-- division left a remainder, decide the rounding exactly.
roundRatio :: Integer -> Integer -> Double
roundRatio numerator denominator
  | binaryExponent >= 1024 = infinity
  | otherwise = encodeFloat rounded lowestBit
  where
    -- scale so that the quotient has at least 56 bits
    scale = max 0 (56 + bitLength denominator - bitLength numerator)
    (quotient, remainder) = (numerator `shiftL` scale) `quotRem` denominator
    width = bitLength quotient
    -- the value is quotient * 2^-scale, so its leading bit is worth 2^binaryExponent
    binaryExponent = width - 1 - scale
    -- the weight of the last bit kept: 52 below the leading one, but never
    -- below the smallest subnormal's
    lowestBit = max (binaryExponent - 52) (-1074)
    dropped = lowestBit + scale
    kept = quotient `shiftR` dropped
    below = quotient .&. ((1 `shiftL` dropped) - 1)
    half = 1 `shiftL` (dropped - 1)
    roundsUp = below > half || (below == half && (remainder /= 0 || odd kept))
    rounded = if roundsUp then kept + 1 else kept

-- | The number of bits in a positive integer.
bitLength :: Integer -> Int
bitLength = go 0
  where
    go acc v
      | v >= 18446744073709551616 = go (acc + 64) (v `shiftR` 64)
      | v == 0 = acc
      | otherwise = go (acc + 1) (v `shiftR` 1)

-- | ECMAScript's @WhiteSpace@ and @LineTerminator@ code points, which is what
-- string-to-number conversion trims. Listed explicitly rather than taken from
-- @Data.Char.isSpace@ because the two differ: U+FEFF, U+2028 and U+2029 are
-- trimmed by JavaScript and not by @isSpace@, while U+0085 and U+180E are
-- trimmed by neither (U+180E stopped being a space in Unicode 6.3).
isJsSpace :: Char -> Bool
isJsSpace c = case fromEnum c of
  0x0009 -> True
  0x000A -> True
  0x000B -> True
  0x000C -> True
  0x000D -> True
  0x0020 -> True
  0x00A0 -> True
  0x1680 -> True
  0x2028 -> True
  0x2029 -> True
  0x202F -> True
  0x205F -> True
  0x3000 -> True
  0xFEFF -> True
  n -> n >= 0x2000 && n <= 0x200A

-- | The string-to-number conversion of JavaScript's @Number(string)@: the
-- whole trimmed string must be a literal (decimal, @Infinity@, or an
-- unsigned @0x@, @0o@ or @0b@ literal); the empty string is zero; anything
-- else is @NaN@.
stringToNumber :: Text -> Double
stringToNumber source
  | null trimmed = 0
  | otherwise = case radixLiteral trimmed of
      Just number -> number
      Nothing -> case decimalPrefix trimmed of
        Just (number, []) -> number
        _ -> nan
  where
    trimmed = dropWhileEnd isJsSpace (dropWhile isJsSpace (Text.unpack source))

-- | JavaScript's @parseFloat@ on a string: the longest decimal prefix after
-- leading whitespace, ignoring what follows; @NaN@ if there is none. Unlike
-- 'stringToNumber' it accepts trailing junk and does not read @0x@ literals.
parseFloatString :: Text -> Double
parseFloatString source =
  case decimalPrefix (dropWhile isJsSpace (Text.unpack source)) of
    Just (number, _) -> number
    Nothing -> nan

-- | @0x..@, @0o..@, @0b..@ literals (unsigned, at least one digit).
radixLiteral :: String -> Maybe Double
radixLiteral ('0' : marker : digits)
  | not (null digits)
  , Just base <- radixOf marker
  , all (isDigitIn base) digits =
      Just (radixValue base digits)
radixLiteral _ = Nothing

-- | The value of a digit string in a power-of-two base. Anything with more
-- than 1100 significant bits overflows a double, and is decided from the
-- digit count so a hostile million-digit literal costs nothing.
radixValue :: Integer -> String -> Double
radixValue base digits
  | length significant * bitsPerDigit > 1100 = infinity
  | otherwise = fromRational (fromInteger (List.foldl' (\acc c -> acc * base + toInteger (digitToInt c)) 0 significant))
  where
    significant = dropWhile (== '0') digits
    bitsPerDigit = case base of
      16 -> 4
      8 -> 3
      _ -> 1

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
    Nothing -> fmap (first applySign) (unsignedDecimal afterSign)
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
              else (withSign isNegative (exponentMagnitude digits), remainder)
  _ -> (0, input)
  where
    withSign flag n = if flag then negate n else n

-- | An exponent's size, saturated at 10^9: every double is decided long
-- before that, and a hostile exponent must not become a huge 'Integer'.
exponentMagnitude :: String -> Integer
exponentMagnitude digits
  | length significant > 9 = 1000000000
  | otherwise = digitsToInteger significant
  where
    significant = dropWhile (== '0') digits

digitsToInteger :: String -> Integer
digitsToInteger = List.foldl' (\acc c -> acc * 10 + toInteger (digitToInt c)) 0

-- | Build the double from decimal digits and a power of ten, correctly
-- rounded (see 'decimalToDouble').
--
-- Only the first 'significantDigits' digits are converted. A longer literal
-- needs no more: 768 significant digits decide the rounding of every double
-- (the longest exact decimal expansion of a halfway case), and any non-zero
-- digit beyond them can only break a tie, which is recorded as one sticky
-- trailing digit. That keeps the cost linear in the length of the literal;
-- converting every digit is quadratic, and a million-digit string used in a
-- number context would otherwise take minutes.
assemble :: String -> String -> Integer -> Double
assemble intDigits fracDigits exponentValue =
  decimalToDouble mantissa (exponentValue - toInteger (length fracDigits) + dropped)
  where
    digits = dropWhile (== '0') (intDigits ++ fracDigits)
    excess = length digits - significantDigits
    (mantissa, dropped)
      | excess <= 0 = (digitsToInteger digits, 0)
      | any (/= '0') (drop significantDigits digits) =
          (digitsToInteger (take significantDigits digits) * 10 + 1, toInteger excess - 1)
      | otherwise = (digitsToInteger (take significantDigits digits), toInteger excess)

-- | Digits kept when converting a decimal literal; see 'assemble'.
significantDigits :: Int
significantDigits = 800

-- ---------------------------------------------------------------------
-- Writing
-- ---------------------------------------------------------------------

-- | JavaScript's @Number::toString@ (ECMAScript, radix 10): the shortest
-- digits that read back as the double, laid out in plain or exponent form.
numberToString :: Double -> Text
numberToString d
  | isNaN d = "NaN"
  | d == 0 = "0"
  | isInfinite d = if d > 0 then "Infinity" else "-Infinity"
  | d < 0 = "-" <> numberToString (negate d)
  | Just whole <- wholeNumber d = Text.pack (show whole)
  | otherwise = Text.pack (layoutDigits digits (length digits) (length digits + exponent10))
  where
    (coefficientDigits, exponent10) = shortestDecimal d
    digits = show coefficientDigits

-- | A finite double as the decimal JavaScript would print for it. Negative
-- zero becomes zero, as @JSON.stringify@ writes it.
doubleToScientific :: Double -> Scientific
doubleToScientific d
  | d == 0 = 0
  | d < 0 = negate (doubleToScientific (negate d))
  | otherwise = uncurry scientific (shortestDecimal d)

-- | A positive double below 2^53 that is a whole number, as an integer.
wholeNumber :: Double -> Maybe Int64
wholeNumber d
  | d < twoTo53 && fromIntegral whole == d = Just whole
  | otherwise = Nothing
  where
    whole = truncate d

-- | The digits ECMAScript prints for a positive finite double, as
-- @(digits, pointPosition)@ in the convention of 'floatToDigits': the value
-- is @0.d1d2...dn * 10^pointPosition@.
shortestDigits :: Double -> ([Int], Int)
shortestDigits d = (map digitToInt digits, length digits + exponent10)
  where
    (coefficientDigits, exponent10) = shortestDecimal d
    digits = show coefficientDigits

-- | The decimal ECMAScript prints for a positive finite double, as
-- @(significand, exponent)@ with the value @significand * 10^exponent@ and no
-- trailing zeros in the significand.
--
-- ECMAScript asks for the fewest digits that read back as the double, and
-- among those the closest to it, and among two equally close the even one.
-- A whole number below 2^53 is its own digits; anything else is Ryū
-- ("JsonLogic.Core.Ryu"), which answers exactly that question.
shortestDecimal :: Double -> (Integer, Int)
shortestDecimal d = case wholeNumber d of
  Just whole -> stripZeros (toInteger whole) 0
  Nothing -> case shortestDecimalRyu d of
    (digits, exponent10) -> stripZeros (toInteger digits) exponent10
  where
    stripZeros m e
      | m /= 0 && m `rem` 10 == 0 = stripZeros (m `quot` 10) (e + 1)
      | otherwise = (m, e)

-- | 'shortestDigits' computed exactly, for every positive finite double.
--
-- GHC's 'floatToDigits' only accepts digit strings strictly inside the
-- rounding interval of the double. ECMAScript also accepts the exact
-- halfway point when the double's significand is even, because
-- round-half-even reads it back to the same double. For doubles of 2^53 and
-- above those points are whole numbers, so a shorter string can exist where
-- GHC needs one more digit: @2.2531e21@, not @2.2531000000000001e21@. Below
-- 2^53 the halfway points have too many digits to matter, so GHC's answer
-- stands, except that when two candidates of the shortest length are equally
-- close (@9007199254740993 / 10@ is halfway between @...099.2@ and @...099.3@)
-- ECMAScript takes the even one, and 'floatToDigits' does not.
shortestDigitsExact :: Double -> ([Int], Int)
shortestDigitsExact d
  | d < twoTo53 =
      -- A tie between two equally short candidates needs 16 or more digits:
      -- below that, decimal spacing is wider than the spacing of doubles.
      if length digits >= 16 then fromMaybe generated (shorter (length digits)) else generated
  | otherwise = fromMaybe generated (listToMaybe (mapMaybe shorter [1 .. length digits]))
  where
    generated@(digits, pointPosition) = floatToDigits 10 d
    exact = toRational d
    shorter k =
      case sortOn rank (filter readsBack [lower, upper]) of
        best : _ -> Just (digitsOf best k)
        [] -> Nothing
      where
        scale = 10 ^^ (pointPosition - k) :: Rational
        scaled = exact / scale
        lower = floor scaled :: Integer
        upper = ceiling scaled :: Integer
        readsBack m = fromRational (fromInteger m * scale) == d
        rank m = (abs (fromInteger m * scale - exact), odd m)
    digitsOf m k = (map digitToInt (dropWhileEnd (== '0') text), pointPosition + length text - k)
      where
        text = show m

-- | @layoutDigits ds k n@: @k@ significant digits @ds@ whose decimal
-- point sits @n@ places to the right of their start, laid out as
-- ECMAScript's @Number::toString@ does.
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
-- Arithmetic
-- ---------------------------------------------------------------------

-- | JavaScript's @%@ on doubles: the truncating remainder, which takes the
-- dividend's sign, including when it is zero (@-4 % 2@ is @-0@). Exact:
-- whole numbers below 2^53 use integer 'rem', anything else is computed in
-- 'Rational'.
remainderJs :: Double -> Double -> Double
remainderJs x y
  | isNaN x || isNaN y || isInfinite x || y == 0 = nan
  | isInfinite y = x
  | x == 0 = x
  | otherwise = signed (magnitude (abs x) (abs y))
  where
    signed r
      | x < 0 = negate r
      | otherwise = r
    magnitude a b = case (wholeNumber a, wholeNumber b) of
      (Just m, Just n) -> fromIntegral (m `rem` n)
      _ ->
        let ra = toRational a
            rb = toRational b
         in fromRational (ra - rb * fromInteger (truncate (ra / rb)))

-- | @Math.max@: @NaN@ if any argument is, @-Infinity@ for none, and
-- @+0@ above @-0@ regardless of argument order.
maximumJs :: [Double] -> Double
maximumJs xs
  | any isNaN xs = nan
  | otherwise = List.foldl' larger (negate infinity) xs
  where
    larger a b
      | a > b = a
      | b > a = b
      | isNegativeZero a = b
      | otherwise = a

-- | @Math.min@: @NaN@ if any argument is, @Infinity@ for none, and
-- @-0@ below @+0@ regardless of argument order.
minimumJs :: [Double] -> Double
minimumJs xs
  | any isNaN xs = nan
  | otherwise = List.foldl' smaller infinity xs
  where
    smaller a b
      | a < b = a
      | b < a = b
      | isNegativeZero a = a
      | otherwise = b
