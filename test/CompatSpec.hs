{-# LANGUAGE OverloadedStrings #-}

-- | Compatibility with the reference where the audit found disagreements:
-- arity modes, UTF-16 strings, signed zero, whitespace, the custom
-- operator registry, and the other behaviours the outlier fixture also
-- covers, each stated here as an example a reader can follow.
module CompatSpec (spec) where

import Data.Aeson (Value (..), decode, object, toJSON, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Test.Hspec
import Test.QuickCheck

import Gen (genText)
import JsonLogic
import JsonLogic.Core.Utf16

-- | A registry that the test expects to be accepted.
registry :: [(OperatorName, CustomOperator)] -> Operators
registry = either (error . show) id . operatorsFromList

json :: String -> Value
json text = fromMaybe (error ("bad test JSON: " ++ text)) (decode (Char8.pack text))

strict, lenient :: String -> String -> Either Error Value
strict rule dat = apply (json rule) (json dat)
lenient rule dat = applyWithConfig defaultConfig{arityMode = Lenient} noOperators (json rule) (json dat)

-- | Is a rule's numeric result negative zero? Observed through 1 / x.
yieldsNegativeZero :: String -> Bool
yieldsNegativeZero rule = strict ("{\"<\":[{\"/\":[1," ++ rule ++ "]},0]}") "null" == Right (Bool True)

spec :: Spec
spec = do
  describe "arity modes" $ do
    it "strict rejects a wrong argument count precisely" $
      strict "{\"==\":[1]}" "null"
        `shouldBe` Left (ParseFailure (ArityMismatch (OperatorName "==") (Exactly 2) 1))
    it "lenient treats a missing argument as undefined" $ do
      lenient "{\"==\":[null]}" "null" `shouldBe` Right (Bool True)
      lenient "{\"==\":[1]}" "null" `shouldBe` Right (Bool False)
    it "lenient ignores a surplus argument" $
      lenient "{\"+\":[1]}" "null" `shouldBe` Right (Number 1)
    it "lenient still evaluates a surplus argument of an ordinary operator, so it can fail" $
      lenient "{\"==\":[1,1,{\"nope\":[]}]}" "null"
        `shouldBe` Left (EvalFailure (UnknownOperator (OperatorName "nope")))
    it "lenient never looks at a surplus argument of map, as in the reference" $
      lenient "{\"map\":[[1],{\"var\":\"\"},{\"nope\":[]}]}" "null" `shouldBe` Right (toJSON [1 :: Int])
    it "lenient reduce without an initial value starts from null" $
      lenient "{\"reduce\":[[1,2],{\"cat\":[{\"var\":\"accumulator\"},{\"var\":\"current\"}]}]}" "null"
        `shouldBe` Right (String "12")
    it "both modes reject * with no operands, as the reference throws" $ do
      strict "{\"*\":[]}" "null" `shouldBe` Left (ParseFailure (ArityMismatch (OperatorName "*") (AtLeast 1) 0))
      lenient "{\"*\":[]}" "null" `shouldBe` Left (ParseFailure (ArityMismatch (OperatorName "*") (AtLeast 1) 0))
    it "strict success implies the same lenient answer" $
      forAll (genRuleForArity 4) $ \rule -> case apply rule Null of
        Right v -> applyWithConfig defaultConfig{arityMode = Lenient} noOperators rule Null === Right v
        Left _ -> property True

  describe "UTF-16 strings" $ do
    it "an emoji has length 2" $
      strict "{\"var\":\"s.length\"}" "{\"s\":\"\\ud83d\\ude00\"}" `shouldBe` Right (Number 2)
    it "a BMP character has length 1" $
      strict "{\"var\":\"s.length\"}" "{\"s\":\"\\u00e9\"}" `shouldBe` Right (Number 1)
    it "indexing inside a pair gives U+FFFD, the one accepted deviation" $
      strict "{\"var\":\"s.0\"}" "{\"s\":\"\\ud83d\\ude00\"}" `shouldBe` Right (String "\xFFFD")
    it "substr on pair boundaries returns the whole character" $
      strict "{\"substr\":[\"a\\ud83d\\ude00b\",1,2]}" "null" `shouldBe` Right (String "\x1F600")
    it "substr through a pair gives U+FFFD for the severed half" $
      strict "{\"substr\":[\"\\ud83d\\ude00\",0,1]}" "null" `shouldBe` Right (String "\xFFFD")
    it "orders an astral character below U+E000, as JavaScript does" $ do
      strict "{\"<\":[\"\\ud83d\\ude00\",\"\\ue000\"]}" "null" `shouldBe` Right (Bool True)
      strict "{\"<\":[\"\\ue000\",\"\\ud83d\\ude00\"]}" "null" `shouldBe` Right (Bool False)
    it "orders astral characters by their code points among themselves" $
      strict "{\"<\":[\"\\ud83d\\ude00\",\"\\ud83d\\ude01\"]}" "null" `shouldBe` Right (Bool True)
    it "utf16Length equals the length of the UTF-16 encoding" $
      forAll genText $
        \t -> utf16Length t === length (utf16Units t)
    it "units round-trip" $
      forAll genText $
        \t -> fromUtf16Units (utf16Units t) === t
    it "compareUtf16 is code unit order" $
      forAll genText $
        \a -> forAll genText $ \b -> compareUtf16 a b === compare (utf16Units a) (utf16Units b)
    it "slices on character boundaries put the text back together" $
      forAll genText $ \t -> forAll (choose (0, Text.length t)) $ \k ->
        let n = utf16Length t
            cut = utf16Length (Text.take k t)
         in utf16Slice 0 cut t <> utf16Slice cut (n - cut) t === t

  describe "signed zero" $ do
    it "Math.max puts +0 above -0 in either order" $ do
      yieldsNegativeZero "{\"max\":[0,{\"*\":[-1,0]}]}" `shouldBe` False
      yieldsNegativeZero "{\"max\":[{\"*\":[-1,0]},0]}" `shouldBe` False
    it "Math.min puts -0 below +0 in either order" $ do
      yieldsNegativeZero "{\"min\":[0,{\"*\":[-1,0]}]}" `shouldBe` True
      yieldsNegativeZero "{\"min\":[{\"*\":[-1,0]},0]}" `shouldBe` True
    it "-0 stays -0 through max and min of itself" $ do
      yieldsNegativeZero "{\"max\":[{\"*\":[-1,0]},{\"*\":[-1,0]}]}" `shouldBe` True
      yieldsNegativeZero "{\"min\":[{\"*\":[-1,0]}]}" `shouldBe` True
    it "a remainder of zero takes the sign of the dividend" $ do
      yieldsNegativeZero "{\"%\":[-4,2]}" `shouldBe` True
      yieldsNegativeZero "{\"%\":[4,-2]}" `shouldBe` False
      yieldsNegativeZero "{\"%\":[-1,1]}" `shouldBe` True
      yieldsNegativeZero "{\"%\":[{\"*\":[-1,0]},5]}" `shouldBe` True
    it "the observation 0 > 1 / max(0, -0) is false, as in JavaScript" $
      strict "{\">\":[0,{\"/\":[1,{\"max\":[0,{\"*\":[-1,0]}]}]}]}" "null" `shouldBe` Right (Bool False)

  describe "string to number whitespace" $ do
    let plus c = strict ("{\"+\":[\"" ++ c ++ "12\"]}") "null"
        times c = strict ("{\"*\":[\"" ++ c ++ "12\",1]}") "null"
    it "trims every ECMAScript WhiteSpace and LineTerminator" $
      mapM_
        (\c -> (plus c, times c) `shouldBe` (Right (Number 12), Right (Number 12)))
        ["\\ufeff", "\\u2028", "\\u2029", "\\u00a0", "\\u1680", "\\u2000", "\\u200a", "\\u202f", "\\u205f", "\\u3000", "\\t", "\\n", "\\u000b", "\\u000c", "\\r", " "]
    it "does not trim near misses" $
      mapM_
        (\c -> (plus c, times c) `shouldBe` (Right Null, Right Null))
        ["\\u0085", "\\u180e", "\\u200b", "\\u001c", "\\u0000", "\\u2060"]

  describe "numbers" $ do
    it "rounds halfway decimal ties to the even digit, as ECMAScript prints" $
      strict "{\"/\":[9007199254740993,10]}" "null" `shouldBe` Right (json "900719925474099.2")
    it "converts a million-digit string without hanging" $ do
      let digits = replicate 1000000 '7'
      applyWithConfig defaultConfig noOperators (json ("{\"+\":[\"" ++ digits ++ "\"]}")) Null
        `shouldBe` Right Null
    it "keeps 768 digits significant and a sticky tail" $ do
      let half n = "0." ++ replicate n '0' ++ "1"
      strict ("{\"+\":[\"" ++ half 400 ++ "\"]}") "null" `shouldBe` Right (Number 0)
    it "reads a hex literal of any length without hanging" $
      strict ("{\"==\":[\"0x" ++ replicate 100000 'f' ++ "\",{\"/\":[1,0]}]}") "null" `shouldBe` Right (Bool True)

  describe "missing and missing_some" $ do
    it "reads an array key as a variable's path and default" $ do
      strict "{\"missing\":[\"q\",[\"a\",\"x\"]]}" "{}" `shouldBe` Right (json "[\"q\"]")
      strict "{\"missing\":[\"q\",[\"a\",null]]}" "{}" `shouldBe` Right (json "[\"q\",[\"a\",null]]")
    it "counts string options by UTF-16 length, as the reference does" $
      strict "{\"missing_some\":[2,\"ab\"]}" "{}" `shouldBe` Right (json "[\"ab\"]")
    it "throws where the reference throws on a null option list" $
      strict "{\"missing_some\":[1,null]}" "{}"
        `shouldBe` Left (EvalFailure (InvalidOperand (OperatorName "missing_some")))
    it "returns the missing keys when the options have no length" $
      strict "{\"missing_some\":[0,0]}" "{}" `shouldBe` Right (json "[0]")

  describe "inherited property names (DEV-INHERITED)" $ do
    it "sees only own properties, where the reference walks the prototype chain" $
      mapM_
        (\name -> strict ("{\"var\":\"" ++ name ++ "\"}") "{}" `shouldBe` Right Null)
        ["constructor", "__proto__", "toString", "hasOwnProperty", "valueOf"]
    it "still finds an own property of that name" $
      strict "{\"var\":\"constructor\"}" "{\"constructor\":7}" `shouldBe` Right (Number 7)

  describe "undefined" $ do
    it "an object property that is undefined is left out, as JSON.stringify does" $
      strict "{\"reduce\":[[0],{\"var\":\"\"},{\"and\":[]}]}" "null" `shouldBe` Right (json "{\"current\":0}")

  describe "custom operators" $ do
    let double :: CustomOperator
        double _ args = case args of
          [Number n] -> Right (Number (n * 2))
          _ -> Left "expected one number"
    it "are refused when named like a builtin, instead of being ignored" $
      case operatorsFromList [(OperatorName "var", double)] of
        Left e -> e `shouldBe` ReservedOperatorName (OperatorName "var")
        Right _ -> expectationFailure "a builtin name was accepted"
    it "are refused when a name is given twice" $
      case operatorsFromList [(OperatorName "double", double), (OperatorName "double", double)] of
        Left e -> e `shouldBe` DuplicateOperatorName (OperatorName "double")
        Right _ -> expectationFailure "a duplicate name was accepted"
    it "compose left-biased" $ do
      let a = registry [(OperatorName "f", \_ _ -> Right (toJSON (1 :: Int)))]
          b = registry [(OperatorName "f", \_ _ -> Right (toJSON (2 :: Int))), (OperatorName "g", \_ _ -> Right (toJSON (3 :: Int)))]
          both = a <> b
      applyWith both (object ["f" .= ([] :: [Int])]) Null `shouldBe` Right (Number 1)
      applyWith both (object ["g" .= ([] :: [Int])]) Null `shouldBe` Right (Number 3)
    it "a custom failure inside map stops the evaluation" $ do
      let ops = registry [(OperatorName "double", double)]
      applyWith ops (json "{\"map\":[[1,\"x\"],{\"double\":[{\"var\":\"\"}]}]}") Null
        `shouldBe` Left (EvalFailure (CustomOperatorFailed (OperatorName "double") "expected one number"))
    it "combining with the empty registry changes nothing" $ do
      let ops = registry [(OperatorName "double", double)]
      applyWith (ops <> noOperators) (json "{\"double\":[2]}") Null `shouldBe` Right (Number 4)

  describe "renderError" $ do
    it "describes each constructor" $ do
      renderError (ParseFailure (DepthExceeded 64)) `shouldBe` "rule nests deeper than the limit of 64"
      renderError (EvalFailure (UnknownOperator (OperatorName "x"))) `shouldBe` "unknown operator \"x\""
      renderError (EvalFailure (LimitExceeded (StepLimit 9))) `shouldBe` "evaluation exceeded the limit of 9 steps"
      renderError (ParseFailure (ArityMismatch (OperatorName "==") (Exactly 2) 1))
        `shouldBe` "operator \"==\" takes exactly 2 arguments, but was given 1"

genRuleForArity :: Int -> Gen Value
genRuleForArity _ = do
  name <- elements ["+", "==", "<", "-", "cat", "max", "!", "if", "and", "merge"]
  k <- choose (0, 3)
  args <- vectorOf k (oneof [toJSON <$> (choose (-3, 3) :: Gen Int), pure Null, String <$> genText])
  pure (object [Key.fromText name .= args])
