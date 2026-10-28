{-# LANGUAGE OverloadedStrings #-}

-- | Behaviour that a forensic review found missing, pinned so it stays
-- fixed: limits that wrap round at 'maxBound', work that the step budget did
-- not see, a rule size that was not bounded, a quadratic number reader, and
-- the contract of the exception and deepseq instances.
module RegressionSpec (spec) where

import Control.DeepSeq (force)
import Control.Exception (Exception, evaluate, fromException, throw, throwIO, try)
import Data.Aeson (Value (..), encode, object, toJSON, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.Either (fromRight)
import Data.Scientific (Scientific, scientific)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Timeout (timeout)
import Test.Hspec
import Test.QuickCheck

import Gen (genData, genRule, genText)
import JsonLogic
import JsonLogic.Core.Number (scientificToDouble)
import JsonLogic.Core.Utf16 (compareUtf16, utf16CharAt, utf16Slice)
import Oracle (charAtOracle, sliceOracle, utf16Units)
import Support (Problem (..), json, problem)

call :: Text -> [Value] -> Value
call operator operands = object [Key.fromText operator .= operands]

variable :: Text -> Value
variable path = object ["var" .= path]

-- | Force the answer; fail the test if it takes more than the given seconds.
withinSeconds :: Int -> Either e Value -> IO (Either e Value)
withinSeconds seconds result = do
  outcome <- timeout (seconds * 1000000) (evaluate (Char8.length (encode (fromRight Null result)) `seq` result))
  maybe (expectationFailure ("did not finish in " ++ show seconds ++ " seconds") >> pure result) pure outcome

-- | A registry of custom operators. The names used here are all valid; if
-- one were not, the registry is empty and the test that uses it fails on an
-- unknown operator.
registry :: [(Text, Value -> [Value] -> Either Text Value)] -> Operators
registry entries =
  fromRight noOperators (operatorsFromList [(OperatorName name, implementation) | (name, implementation) <- entries])

-- | Compile a rule and hand it to the test, which fails if it does not compile.
-- | What a custom operator throws in the tests below.
data Boom = Boom
  deriving (Eq, Show)

instance Exception Boom

withRule :: Operators -> Value -> (Rule -> IO ()) -> IO ()
withRule operators rule test = case compile defaultConfig operators rule of
  Left e -> expectationFailure ("did not compile: " ++ Text.unpack (renderError e))
  Right ready -> test ready

-- | Hand the test the error of an application, which fails if it succeeded.
withFailure :: Either Error Value -> (Error -> IO ()) -> IO ()
withFailure result test = either test (const (expectationFailure "expected a failure")) result

isStepLimit :: Either Problem Value -> Bool
isStepLimit (Left (EvalProblem (LimitExceeded (StepLimit _)))) = True
isStepLimit _ = False

spec :: Spec
spec = describe "regressions" $ do
  describe "a limit of maxBound means no limit" $ do
    let dat = json "{\"a\":1,\"s\":\"hello\",\"xs\":[1,2]}"
        rules =
          [ ("var", variable "a", Number 1)
          , ("var into a string", variable "s.length", Number 5)
          , ("arithmetic on a var", call "+" [variable "a", Number 1], Number 2)
          , ("missing", call "missing" ["a"], toJSON ([] :: [Int]))
          , ("map", call "map" [variable "xs", call "+" [variable "", Number 1]], toJSON [2, 3 :: Int])
          , ("a string used as a number", call "+" ["12"], Number 12)
          , ("an array converted to a string", call "cat" [variable "xs"], String "1,2")
          ]
    mapM_
      ( \(name, rule, expected) ->
          it name $ do
            problem (applyWithConfig defaultConfig{maxSteps = maxBound} noOperators rule dat) `shouldBe` Right expected
            problem (applyWithConfig defaultConfig{maxSteps = maxBound - 1} noOperators rule dat) `shouldBe` Right expected
      )
      rules
    it "gives the answer a large finite limit gives, for any rule" $
      forAll (genRule 5) $ \rule -> forAll (genData 3) $ \dat' ->
        let unlimited = applyWithConfig defaultConfig{maxSteps = maxBound, arityMode = Lenient} noOperators rule dat'
            large = applyWithConfig defaultConfig{maxSteps = 100000000, arityMode = Lenient} noOperators rule dat'
         in unlimited === large
    it "holds for the other limits too" $ do
      let config = defaultConfig{maxStringLength = maxBound, maxArrayLength = maxBound, maxDepth = maxBound, maxNodes = maxBound}
      problem (applyWithConfig config noOperators (call "cat" ["a", "b"]) Null) `shouldBe` Right (String "ab")

  describe "the size of the rule" $ do
    -- [1,2] is three nodes: the array and its two elements
    it "is held to maxNodes, exactly" $ do
      let rule = json "[1,2]"
      problem (applyWithConfig defaultConfig{maxNodes = 3} noOperators rule Null) `shouldBe` Right (toJSON [1, 2 :: Int])
      problem (applyWithConfig defaultConfig{maxNodes = 2} noOperators rule Null) `shouldBe` Left (ParseProblem (NodeLimitExceeded 2))
    it "refuses a huge rule before building any of it, at the root" $ do
      let wide = object ["+" .= replicate 1500000 (1 :: Int)]
      case compile defaultConfig noOperators wide of
        Left (ParseFailure location (NodeLimitExceeded 1000000)) -> renderLocation location `shouldBe` ""
        other -> expectationFailure ("expected NodeLimitExceeded, got " ++ either (Text.unpack . renderError) (const "a rule") other)

  describe "work on strings is paid for" $ do
    let big = String (Text.replicate 1000000 "a")
        dat = object ["big" .= big, "big2" .= big]
        repeated operator n = call "+" (replicate n operator)
    it "a comparison of long strings" $ do
      r <- withinSeconds 10 (problem (applyWithConfig defaultConfig noOperators (repeated (call "<" [variable "big", variable "big2"]) 1000) dat))
      r `shouldSatisfy` isStepLimit
    it "a substring of a long string" $ do
      r <- withinSeconds 10 (problem (applyWithConfig defaultConfig noOperators (repeated (call "substr" [variable "big", Number 0, Number 1]) 1000) dat))
      r `shouldSatisfy` isStepLimit
    it "a search of a long string" $ do
      r <- withinSeconds 10 (problem (applyWithConfig defaultConfig noOperators (repeated (call "in" ["b", variable "big"]) 1000) dat))
      r `shouldSatisfy` isStepLimit
    it "a long string read as a number" $ do
      let digits = String (Text.replicate 1000000 "7")
      r <- withinSeconds 10 (problem (applyWithConfig defaultConfig noOperators (repeated (call "+" [variable "digits"]) 1000) (object ["digits" .= digits])))
      r `shouldSatisfy` isStepLimit
    it "strict equality of long strings" $ do
      r <- withinSeconds 10 (problem (applyWithConfig defaultConfig noOperators (repeated (call "===" [variable "big", variable "big2"]) 1000) dat))
      r `shouldSatisfy` isStepLimit
    it "does not charge short strings anything extra" $
      problem (applyWithConfig defaultConfig{maxSteps = 8} noOperators (call "<" ["abc", "abd"]) Null) `shouldBe` Right (Bool True)
    it "still lets one use of a long string through" $ do
      r <- withinSeconds 10 (problem (applyWithConfig defaultConfig noOperators (call "<" [variable "big", variable "big2"]) dat))
      r `shouldBe` Right (Bool False)

  describe "UTF-16 strings" $ do
    it "slicing agrees with the list-of-code-units definition" $
      forAll genText $ \t -> forAll (choose (-2, 24)) $ \from -> forAll (choose (-2, 24)) $ \count ->
        utf16Slice from count t === sliceOracle from count t
    it "indexing agrees with the list-of-code-units definition" $
      forAll genText $ \t -> forAll (choose (-2, 24)) $ \i ->
        utf16CharAt i t === charAtOracle i t
    it "ordering agrees with comparing code-unit lists" $
      forAll genText $ \a -> forAll genText $ \b ->
        compareUtf16 a b === compare (utf16Units a) (utf16Units b)
    it "ordering is right when the strings share a long prefix and differ in a character's last byte" $ do
      compareUtf16 "\233" "\232" `shouldBe` GT
      compareUtf16 (Text.replicate 100 "a" <> "\x1F600") (Text.replicate 100 "a" <> "\xFFFF") `shouldBe` LT

  describe "numbers" $ do
    -- 77...7, built in linear time
    let digitsOf n = 7 * ((10 ^ n - 1) `div` 9) :: Integer
        decimal :: Int -> Int -> Scientific
        decimal digits = scientific (digitsOf digits)
    it "a long JSON number reads as the nearest double" $
      mapM_
        (\(digits, power) -> scientificToDouble (decimal digits power) `shouldBe` fromRational (toRational (decimal digits power)))
        [(30, -30), (400, -405), (800, -790), (2000, -2005), (5000, -4990), (3000, 100), (2000, -2400), (600, -1000)]
    it "a million-digit JSON number is read in linear time" $ do
      let number = decimal 1000000 (-1000000)
      outcome <- timeout 10000000 (evaluate (scientificToDouble number))
      outcome `shouldSatisfy` (/= Nothing)

  describe "evaluation as a value" $ do
    it "a custom operator is handed the root datum as it came" $ do
      let operators = registry [("echo", \datum _ -> Right datum)]
          dat' = json "{\"k\":[1,2,{\"z\":null}]}"
      applyWith operators (call "echo" []) dat' `shouldBe` Right dat'
    it "a custom operator inside map is handed the current element" $ do
      let operators = registry [("echo", \datum _ -> Right datum)]
      applyWith operators (call "map" [toJSON [1, 2 :: Int], call "echo" []]) Null `shouldBe` Right (toJSON [1, 2 :: Int])
    it "compile then run equals apply, and a compiled rule is already in normal form" $
      forAll (genRule 5) $ \rule -> forAll (genData 3) $ \dat' ->
        case compile defaultConfig noOperators rule of
          Left e -> apply rule dat' === Left e
          Right ready -> force ready `seq` (run ready dat' === apply rule dat')

  describe "exceptions and instances" $ do
    it "tryRun catches an exception thrown by a custom operator" $ do
      let operators = registry [("boom", \_ _ -> throw Boom)]
      withRule operators (call "boom" []) $ \ready -> do
        r <- tryRun ready Null
        case r of
          Left e -> fromException e `shouldBe` Just Boom
          Right _ -> expectationFailure "the exception escaped tryRun's result"
    it "tryRun returns an ordinary answer unchanged" $ do
      withRule noOperators (call "+" [Number 1, Number 2]) $ \ready -> do
        r <- tryRun ready Null
        either (const Nothing) Just r `shouldBe` Just (Right (Number 3))
    it "an Error can be thrown and caught" $ do
      withFailure (apply (call "nope" []) Null) $ \e -> do
        r <- try (throwIO e) :: IO (Either Error ())
        r `shouldBe` Left e
    it "an Error can be forced" $ do
      withFailure (apply (call "nope" []) Null) $ \e ->
        force e `shouldBe` e
