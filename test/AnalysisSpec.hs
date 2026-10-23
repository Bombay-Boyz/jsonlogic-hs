{-# LANGUAGE OverloadedStrings #-}

-- | 'dataUse': examples, and the property that makes it worth having: when
-- it says a rule reads only some paths, changing anything else in the data
-- changes nothing about the answer.
module AnalysisSpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec
import Test.QuickCheck

import Gen (genData, genRule)
import JsonLogic
import Support (json)

uses :: String -> Either Error DataUse
uses rule = dataUse <$> compile defaultConfig{arityMode = Lenient} noOperators (json rule)

reads' :: [Text] -> Either Error DataUse
reads' paths = Right (DataUse paths False)

-- | The top-level keys the generated data uses, and one it never does.
topKeys :: [Text]
topKeys = ["a", "b", "l", "unused"]

firstSegment :: Text -> Text
firstSegment = Text.takeWhile (/= '.')

spec :: Spec
spec = describe "dataUse" $ do
  describe "examples" $ do
    it "lists literal var paths, with defaults that are rules too" $
      uses "{\"+\":[{\"var\":\"a.b\"},{\"var\":[\"c\",{\"var\":\"d\"}]}]}" `shouldBe` reads' ["a.b", "c", "d"]
    it "does not count what an iteration body reads, which is the element" $
      uses "{\"map\":[{\"var\":\"xs\"},{\"+\":[{\"var\":\"\"},{\"var\":\"y\"}]}]}" `shouldBe` reads' ["xs"]
    it "counts reduce's initial value, which is evaluated against the data" $
      uses "{\"reduce\":[{\"var\":\"xs\"},{\"var\":\"current\"},{\"var\":\"start\"}]}" `shouldBe` reads' ["start", "xs"]
    it "lists the keys of missing and missing_some" $ do
      uses "{\"missing\":[\"a\",\"b\"]}" `shouldBe` reads' ["a", "b"]
      uses "{\"missing\":[[\"a\",\"b\"],\"ignored\"]}" `shouldBe` reads' ["a", "b"]
      uses "{\"missing_some\":[1,[\"a\",\"b\"]]}" `shouldBe` reads' ["a", "b"]
    it "reads a numeric path as the string var would use" $
      uses "{\"var\":1}" `shouldBe` reads' ["1"]
    it "gives up, explicitly, on what cannot be known" $
      mapM_
        (\rule -> fmap usesAllData (uses rule) `shouldBe` Right True)
        [ "{\"var\":\"\"}"
        , "{\"var\":[]}"
        , "{\"var\":{\"cat\":[\"a\",\"b\"]}}"
        , "{\"missing\":{\"var\":\"keys\"}}"
        , "{\"missing_some\":[1,{\"var\":\"keys\"}]}"
        , "{\"custom\":[1]}"
        ]
    it "reads nothing from a rule with no lookups" $
      uses "{\"if\":[true,1,2]}" `shouldBe` reads' []

  it "a rule's answer does not change when data it does not read changes" $
    checkCoverage $
      forAll (genRule 6) $ \rule -> forAll (genData 3) $ \dat -> forAll (genData 2) $ \replacement ->
        forAll (elements [defaultConfig, defaultConfig{arityMode = Lenient}, defaultConfig{maxSteps = 40}]) $ \config ->
          case (compile config noOperators rule, dat) of
            (Right compiled, Object fields)
              | DataUse paths False <- dataUse compiled ->
                  let unread = [k | k <- topKeys, k `notElem` map firstSegment paths]
                      changed k = Object (KeyMap.insert (Key.fromText k) replacement fields)
                      removed k = Object (KeyMap.delete (Key.fromText k) fields)
                   in cover 5 (any (`elem` ["a", "b", "l"]) unread) "a key the data has goes unread" $
                        conjoin
                          [ counterexample (show (k, paths)) (run compiled other === run compiled dat)
                          | k <- unread
                          , other <- [changed k, removed k]
                          ]
            _ -> cover 5 False "a key the data has goes unread" (property True)
