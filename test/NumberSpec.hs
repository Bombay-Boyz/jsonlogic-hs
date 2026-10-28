{-# LANGUAGE OverloadedStrings #-}

-- | The fast paths of "JsonLogic.Core.Number" against the exact general
-- paths they stand in for. Each fast path is used only when it is provably
-- exact; these properties check that claim over hundreds of thousands of
-- doubles, chosen to include the hard cases (whole numbers at 2^53 and
-- above, short decimals, numbers needing 16 or 17 digits, subnormals).
module NumberSpec (spec) where

import Data.Bits (shiftR, (.&.))
import Data.Word (Word64)
import GHC.Float (castWord64ToDouble)
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck hiding ((.&.))

import JsonLogic.Core.Number
import Oracle (shortestDigits, shortestDigitsExact)

-- | Any positive finite double, from its bit pattern.
genAnyDouble :: Gen Double
genAnyDouble = (castWord64ToDouble . (.&. 0x7FFFFFFFFFFFFFFF) <$> (arbitrary :: Gen Word64)) `suchThat` finitePositive

-- | Doubles of the kinds rules usually hold: short decimals, money, ratios,
-- and whole numbers either side of 2^53.
genCommonDouble :: Gen Double
genCommonDouble =
  oneof
    [ (/ 100) . fromInteger <$> choose (1, 10000000)
    , (/ 1000) . fromInteger <$> choose (1, 100000)
    , (\a b -> fromInteger a / fromInteger b) <$> choose (1, 1000000) <*> choose (1, 1000)
    , fromInteger <$> choose (1, 2 ^ (60 :: Int))
    , (* 1e-9) . fromInteger <$> choose (1, 1000000)
    , (\w -> fromIntegral (w `shiftR` 11 :: Word64)) <$> arbitrary
    ]
    `suchThat` finitePositive

finitePositive :: Double -> Bool
finitePositive d = d > 0 && not (isInfinite d) && not (isNaN d)

-- | The slow, obviously correct remainder: exact in 'Rational'.
remainderExact :: Double -> Double -> Double
remainderExact x y
  | isNaN x || isNaN y || isInfinite x || y == 0 = 0 / 0
  | isInfinite y = x
  | x == 0 = x
  | exact == 0 = if x < 0 then -0.0 else 0
  | otherwise = fromRational exact
  where
    rx = toRational x
    ry = toRational y
    exact = rx - ry * fromInteger (truncate (rx / ry))

-- | Run a property this many times instead of the usual hundred.
times :: Int -> SpecWith a -> SpecWith a
times n = modifyMaxSuccess (const n)

sameDouble :: Double -> Double -> Bool
sameDouble a b = (isNaN a && isNaN b) || (a == b && isNegativeZero a == isNegativeZero b)

spec :: Spec
spec = describe "JsonLogic.Core.Number" $ do
  describe "shortest digits" $ do
    times 100000 $ it "the fast path agrees with the exact algorithm on any double" $ forAll genAnyDouble $ \d -> shortestDigits d === shortestDigitsExact d
    times 100000 $ it "the fast path agrees with the exact algorithm on common doubles" $ forAll genCommonDouble $ \d -> shortestDigits d === shortestDigitsExact d
    it "accepts the halfway point above 2^53, as ECMAScript does" $
      numberToString 2.2531e21 `shouldBe` "2.2531e+21"
    it "takes the even digit when two candidates are equally close" $
      numberToString (9007199254740993 / 10) `shouldBe` "900719925474099.2"
    it "writes the extremes" $
      map numberToString [5e-324, 1.7976931348623157e308, 0.1, 123.456, 1e21, 1e-7]
        `shouldBe` ["5e-324", "1.7976931348623157e+308", "0.1", "123.456", "1e+21", "1e-7"]

  describe "reading decimals" $ do
    times 100000 $
      it "the fast path is exact for every mantissa below 2^53 and exponent within 22" $
        forAll (choose (1, 2 ^ (53 :: Int) - 1)) $ \m -> forAll (choose (-22, 22)) $ \e ->
          decimalToDouble m e === fromRational (fromInteger m * 10 ^^ e)
    times 20000 $
      it "is exact beyond the fast path too" $
        forAll (choose (1, 10 ^ (30 :: Int))) $ \m -> forAll (choose (-340, 320)) $ \e ->
          decimalToDouble m e === fromRational (fromInteger m * 10 ^^ e)
    times 50000 $
      it "is exact for the 16- to 19-digit decimals JavaScript and JSON writers produce" $
        forAll (choose (10 ^ (15 :: Int), 10 ^ (19 :: Int))) $ \m -> forAll (choose (-40, 30)) $ \e ->
          decimalToDouble m e === fromRational (fromInteger m * 10 ^^ e)
    times 20000 $
      it "rounds correctly into and out of the subnormal range" $
        forAll (choose (1, 10 ^ (20 :: Int))) $ \m -> forAll (choose (-345, -300)) $ \e ->
          decimalToDouble m e === fromRational (fromInteger m * 10 ^^ e)
    it "rounds an exact tie to even" $ do
      -- 2^53 + 1 is halfway between two doubles; 2^53 + 3 rounds up to 2^53 + 4
      decimalToDouble 9007199254740993 0 `shouldBe` 9007199254740992
      decimalToDouble 9007199254740995 0 `shouldBe` 9007199254740996
      decimalToDouble 90071992547409930 (-1) `shouldBe` 9007199254740992
    it "overflows to infinity and underflows to zero" $ do
      isInfinite (decimalToDouble 17976931348623159 292) `shouldBe` True
      decimalToDouble 17976931348623157 292 `shouldBe` 1.7976931348623157e308
      decimalToDouble 24703282292062327 (-340) `shouldBe` 0
      decimalToDouble 24703282292062328 (-340) `shouldBe` 5e-324
    times 50000 $ it "reads back every double it writes" $ forAll genAnyDouble $ \d -> stringToNumber (numberToString d) === d

  describe "remainder" $ do
    times 50000 $
      it "the integer path agrees with the exact remainder" $
        forAll (choose (negate (2 ^ (53 :: Int)), 2 ^ (53 :: Int))) $ \x -> forAll (choose (-100000, 100000)) $ \y ->
          let (a, b) = (fromInteger x, fromInteger y)
           in counterexample (show (a, b)) (sameDouble (remainderJs a b) (remainderExact a b))
    times 50000 $
      it "agrees with the exact remainder on any doubles" $
        forAll ((,) <$> genAnyDouble <*> genAnyDouble) $ \(a, b) ->
          conjoin
            [ counterexample (show (x, y)) (sameDouble (remainderJs x y) (remainderExact x y))
            | x <- [a, negate a]
            , y <- [b, negate b]
            ]
