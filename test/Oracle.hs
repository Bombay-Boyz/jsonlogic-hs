-- |
-- Module      : Oracle
--
-- Slow, simple or test-only definitions that the tests hold the library to.
-- They used to live in the library, where nothing but the tests used them.
module Oracle
  ( -- * Numbers
    shortestDigits
  , shortestDigitsExact

    -- * UTF-16
  , utf16Units
  , fromUtf16Units
  , sliceOracle
  , charAtOracle

    -- * Rules
  , Term
  , stripLocations
  , parseRule
  , evaluate
  ) where

import Data.Aeson (Value)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Char (chr, digitToInt, ord)
import Data.Fix (Fix (..), foldFix)
import Data.List (dropWhileEnd, sortOn)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word16)
import Numeric (floatToDigits)

import JsonLogic.Core.Config (defaultConfig)
import JsonLogic.Core.Custom (noOperators)
import JsonLogic.Core.Errors (EvalError, ParseError)
import JsonLogic.Core.Eval (compileRule, runProgram)
import JsonLogic.Core.Location (Location)
import JsonLogic.Core.Number (shortestDecimal)
import JsonLogic.Core.Parse (parseRuleWith)
import JsonLogic.Core.Rule (Node (..), Rule, RuleF)

twoTo53 :: Double
twoTo53 = 9007199254740992

-- ---------------------------------------------------------------------
-- Numbers
-- ---------------------------------------------------------------------

-- | The digits ECMAScript prints for a positive finite double, as
-- @(digits, pointPosition)@ in the convention of 'floatToDigits': the value
-- is @0.d1d2...dn * 10^pointPosition@.
shortestDigits :: Double -> ([Int], Int)
shortestDigits d = (map digitToInt digits, length digits + exponent10)
  where
    (coefficientDigits, exponent10) = shortestDecimal d
    digits = show coefficientDigits

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

-- ---------------------------------------------------------------------
-- UTF-16, as lists of code units
-- ---------------------------------------------------------------------

-- | The UTF-16 code units of a text.
utf16Units :: Text -> [Word16]
utf16Units = concatMap charUnits . Text.unpack

charUnits :: Char -> [Word16]
charUnits c
  | n <= 0xFFFF = [fromIntegral n]
  | otherwise =
      let m = n - 0x10000
       in [ fromIntegral (0xD800 .|. (m `shiftR` 10))
          , fromIntegral (0xDC00 .|. (m .&. 0x3FF))
          ]
  where
    n = ord c

-- | Rebuild a text from UTF-16 code units. A surrogate with no partner
-- becomes U+FFFD.
fromUtf16Units :: [Word16] -> Text
fromUtf16Units = Text.pack . go
  where
    go [] = []
    go (hi : lo : rest)
      | isHigh hi && isLow lo = pair hi lo : go rest
    go (u : rest)
      | isHigh u || isLow u = '\xFFFD' : go rest
      | otherwise = chr (fromIntegral u) : go rest
    isHigh u = u >= 0xD800 && u <= 0xDBFF
    isLow u = u >= 0xDC00 && u <= 0xDFFF
    pair hi lo =
      chr
        ( 0x10000
            + ((fromIntegral hi - 0xD800) `shiftL` 10)
            + (fromIntegral lo - 0xDC00)
        )

-- | The slice the library computes, by way of a list of code units.
sliceOracle :: Int -> Int -> Text -> Text
sliceOracle from count text
  | count <= 0 = Text.empty
  | otherwise = fromUtf16Units (take count (drop from (utf16Units text)))

-- | The code unit at an index, by way of a list of code units.
charAtOracle :: Int -> Text -> Maybe Text
charAtOracle i text
  | i < 0 = Nothing
  | otherwise = case drop i (utf16Units text) of
      [] -> Nothing
      unit : _ -> Just (fromUtf16Units [unit])

-- ---------------------------------------------------------------------
-- Rules
-- ---------------------------------------------------------------------

-- | A rule tree without locations, for comparing shapes.
type Term = Fix RuleF

-- | Forget where every node came from.
stripLocations :: Rule -> Term
stripLocations = foldFix (Fix . nodeRule)

-- | Read a rule with 'defaultConfig'.
parseRule :: Value -> Either (Location, ParseError) Rule
parseRule = parseRuleWith defaultConfig

-- | Compile and run with 'defaultConfig' and no custom operators.
evaluate :: Rule -> Value -> Either (Location, EvalError) Value
evaluate = runProgram . compileRule defaultConfig noOperators
