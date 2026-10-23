{-# LANGUAGE OverloadedStrings #-}

module ValueSpec (spec) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec
import Test.QuickCheck (Gen, arbitrary, choose, elements, forAll, listOf, oneof, (==>))

import qualified Data.Vector as Vector

import JsonLogic.Core.Number (maximumJs, minimumJs, numberToString, remainderJs)
import JsonLogic.Core.Value

arr :: [Val] -> Val
arr = VArr . Vector.fromList

nan :: Double
nan = 0 / 0

infinity :: Double
infinity = 1 / 0

genScalar :: Gen Val
genScalar =
  oneof
    [ pure VNull
    , pure VUndefined
    , VBool <$> arbitrary
    , VNum <$> oneof [elements [0, -0.0, 1, -1, 0.5, infinity, negate infinity, nan], arbitrary]
    , VStr . Text.pack <$> listOf (elements "ab01 .-eE")
    ]

genVal :: Gen Val
genVal = oneof [genScalar, arr <$> listOf genScalar, pure (VObj KeyMap.empty)]

numberCases :: [(Double, Text)]
numberCases =
  [ (1, "1")
  , (-1, "-1")
  , (1.5, "1.5")
  , (100, "100")
  , (0.5, "0.5")
  , (0.000001, "0.000001")
  , (1e-7, "1e-7")
  , (1e21, "1e+21")
  , (1e20, "100000000000000000000")
  , (123.456, "123.456")
  , (0.1 + 0.2, "0.30000000000000004")
  , (-0.0, "0")
  , (1.2345e25, "1.2345e+25")
  , (5e-324, "5e-324")
  , (2.2531e21, "2.2531e+21")
  , (-2.2531e21, "-2.2531e+21")
  , (9007199254740992, "9007199254740992")
  , (nan, "NaN")
  , (infinity, "Infinity")
  , (negate infinity, "-Infinity")
  ]

toNumberCases :: [(Val, Double)]
toNumberCases =
  [ (VStr "", 0)
  , (VStr "  12  ", 12)
  , (VStr "0x10", 16)
  , (VStr "0b11", 3)
  , (VStr "1e3", 1000)
  , (VStr ".5", 0.5)
  , (VStr "5.", 5)
  , (VStr "-2.5", -2.5)
  , (VNull, 0)
  , (VBool True, 1)
  , (arr [], 0)
  , (arr [VNum 7], 7)
  , (VStr "Infinity", infinity)
  , (VStr "-Infinity", negate infinity)
  , (VStr "-2.2531e+21", -2.2531e21)
  , (VStr "2.2531e21", 2.2531e21)
  , (VStr "1e400", infinity)
  , (VStr "1e-400", 0)
  , (VStr "5e-324", 5e-324)
  , (VStr "123456789012345678901234567890", 1.2345678901234568e29)
  ]

parseFloatCases :: [(Val, Double)]
parseFloatCases =
  [ (VStr "3.14abc", 3.14)
  , (VStr "  7", 7)
  , (VStr "1e3x", 1000)
  , (VStr ".5", 0.5)
  , (VStr "-12.5e1", -125)
  , (arr [VNum 5], 5)
  , (VNum (-0.0), 0)
  , (VStr "Infinityx", infinity)
  ]

looseCases :: [(Val, Val, Bool)]
looseCases =
  [ (VNum 1, VStr "1", True)
  , (VNum 0, VStr "", True)
  , (VNull, VNull, True)
  , (VNull, VNum 0, False)
  , (VNull, VBool False, False)
  , (VBool True, VNum 1, True)
  , (VBool False, VStr "0", True)
  , (VStr "a", VStr "a", True)
  , (VStr "abc", VNum 0, False)
  , (arr [VNum 1], VStr "1", True)
  , (arr [], VNum 0, True)
  , (arr [], VBool False, True)
  , (arr [], arr [], False)
  , (VNum nan, VNum nan, False)
  , (VUndefined, VNull, True)
  , (VUndefined, VUndefined, True)
  , (VUndefined, VNum 0, False)
  , (VUndefined, VBool False, False)
  ]

lessCases :: [(Val, Val, Bool)]
lessCases =
  [ (VNum 1, VNum 2, True)
  , (VStr "10", VStr "9", True)
  , (VStr "2", VNum 10, True)
  , (VNull, VNum 1, True)
  , (VNum nan, VNum 1, False)
  , (VStr "a", VNum 1, False)
  , (arr [VNum 1], arr [VNum 2], True)
  ]

remainderCases :: [(Double, Double, Double)]
remainderCases =
  [ (5, 3, 2)
  , (-5, 3, -2)
  , (5, -3, 2)
  , (5.5, 2, 1.5)
  , (5, infinity, 5)
  ]

-- | Doubles across the whole range, including the large ones where digit
-- generation has edge cases.
genWideDouble :: Gen Double
genWideDouble = do
  mantissa <- choose (-1000000000000000, 1000000000000000) :: Gen Integer
  power <- choose (-300, 290) :: Gen Int
  pure (fromInteger mantissa * 10 ^^ power)

roundTripsThroughJson :: Double -> Bool
roundTripsThroughJson d = fromValue (toValue (VNum d)) == VNum d

spec :: Spec
spec = describe "JsonLogic.Core.Value" $ do
  describe "truthy" $ do
    it "is false for exactly the reference's falsy values" $
      map truthy [VNull, VUndefined, VBool False, VNum 0, VNum nan, VStr "", arr []]
        `shouldBe` replicate 7 False
    it "is true for everything else, including \"0\" and empty objects" $
      map truthy [VBool True, VNum 1, VNum (-1), VStr "0", VStr "a", arr [VNull], VObj KeyMap.empty]
        `shouldBe` replicate 7 True

  describe "numberToString"
    $ it "follows the ECMAScript Number-to-string rules"
    $ map (numberToString . fst) numberCases `shouldBe` map snd numberCases

  describe "toNumber" $ do
    it "converts the way Number() does" $
      map (toNumber . fst) toNumberCases `shouldBe` map snd toNumberCases
    it "gives NaN for malformed input" $
      map
        (isNaN . toNumber)
        [ VStr "abc"
        , VStr "1 2"
        , VStr "0x"
        , VStr "--1"
        , VStr "1e"
        , VStr "+-1"
        , VStr "infinity"
        , VObj KeyMap.empty
        , arr [VNum 1, VNum 2]
        ]
        `shouldBe` replicate 9 True

  describe "parseFloat" $ do
    it "reads the longest decimal prefix" $
      map (parseFloat . fst) parseFloatCases `shouldBe` map snd parseFloatCases
    it "gives NaN when there is no number to read" $
      map (isNaN . parseFloat) [VStr "abc", VStr "", VStr "x1", VNull, VBool True]
        `shouldBe` replicate 5 True

  describe "equality" $ do
    it "looseEquals matches JavaScript's == on a table of cases" $
      map (\(x, y, _) -> looseEquals x y) looseCases
        `shouldBe` map (\(_, _, expected) -> expected) looseCases
    it "strictEquals never converts types" $
      [ strictEquals (VNum 1) (VStr "1")
      , strictEquals (VNum nan) (VNum nan)
      , strictEquals (VStr "a") (VStr "a")
      , strictEquals (arr []) (arr [])
      ]
        `shouldBe` [False, False, True, False]

  describe "ordering" $ do
    it "lessThan matches JavaScript's < on a table of cases" $
      map (\(x, y, _) -> lessThan x y) lessCases
        `shouldBe` map (\(_, _, expected) -> expected) lessCases
    it "lessOrEqual compares strings lexicographically" $
      [lessOrEqual (VNum 1) (VNum 1), lessOrEqual (VStr "b") (VStr "a")]
        `shouldBe` [True, False]

  describe "arithmetic helpers" $ do
    it "remainderJs takes the sign of the dividend" $
      map (\(x, y, _) -> remainderJs x y) remainderCases
        `shouldBe` map (\(_, _, expected) -> expected) remainderCases
    it "remainderJs gives NaN where JavaScript does" $
      map (isNaN . uncurry remainderJs) [(5, 0), (infinity, 3), (nan, 1)]
        `shouldBe` replicate 3 True
    it "maximumJs and minimumJs follow Math.max and Math.min" $ do
      (maximumJs [1, 3, 2], maximumJs [], minimumJs []) `shouldBe` (3, negate infinity, infinity)
      isNaN (maximumJs [1, nan]) `shouldBe` True

  describe "JSON conversion" $ do
    it "turns NaN and infinities into null" $
      map (toValue . VNum) [nan, infinity, negate infinity]
        `shouldBe` replicate 3 Aeson.Null
    it "keeps ordinary numbers and strings" $ do
      toValue (VNum 2) `shouldBe` Aeson.Number 2
      fromValue (Aeson.String "x") `shouldBe` VStr "x"

  describe "laws" $ do
    it "looseEquals is symmetric" $
      forAll genVal $
        \x -> forAll genVal $ \y -> looseEquals x y == looseEquals y x
    it "strictEquals implies looseEquals" $
      forAll genVal $
        \x -> forAll genVal $ \y -> not (strictEquals x y) || looseEquals x y
    it "lessThan is asymmetric" $
      forAll genVal $
        \x -> forAll genVal $ \y -> not (lessThan x y && lessThan y x)
    it "numberToString round-trips through toNumber" $
      forAll (arbitrary :: Gen Double) $ \d ->
        not (isNaN d || isInfinite d) ==> toNumber (VStr (numberToString d)) == d
    it "writing a double as JSON and reading it back is the identity" $
      forAll genWideDouble roundTripsThroughJson
    it "numberToString round-trips for large and tiny doubles too" $
      forAll genWideDouble $
        \d -> toNumber (VStr (numberToString d)) == d
