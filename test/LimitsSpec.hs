{-# LANGUAGE OverloadedStrings #-}

-- | The resource limits: boundary triples (limit - 1, limit, limit + 1)
-- for every limit, the amplification rules that would otherwise hang or
-- exhaust memory, and properties relating the limits to results.
module LimitsSpec (spec) where

import Control.Exception (evaluate)
import Data.Aeson (Value (..), decode, encode, object, toJSON, (.=))
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.Either (fromRight)
import Data.Maybe (fromMaybe)
import System.Timeout (timeout)
import Test.Hspec
import Test.QuickCheck

import Gen (genData, genRule)
import JsonLogic

json :: String -> Value
json text = fromMaybe (error ("bad test JSON: " ++ text)) (decode (Char8.pack text))

run' :: Config -> String -> Value -> Either Error Value
run' config rule = applyWithConfig config noOperators (json rule)

-- | Force the whole answer, and fail the test if it takes over ten seconds.
within10s :: Either Error Value -> IO (Either Error Value)
within10s result = do
  outcome <- timeout 10000000 (evaluate (Char8.length (encode (fromRight Null result)) `seq` result))
  maybe (expectationFailure "evaluation did not finish in 10 seconds" >> pure result) pure outcome

nestedMap :: Int -> Value
nestedMap levels = foldr (\_ inner -> object ["map" .= [toJSON [1 .. 10 :: Int], inner]]) (object ["var" .= ("" :: String)]) [1 .. levels]

spec :: Spec
spec = describe "resource limits" $ do
  describe "steps" $ do
    -- {"+":[1,1]} is three nodes: the + and its two operands
    it "allows exactly the budget" $
      run' defaultConfig{maxSteps = 3} "{\"+\":[1,1]}" Null `shouldBe` Right (Number 2)
    it "rejects one step short" $
      run' defaultConfig{maxSteps = 2} "{\"+\":[1,1]}" Null
        `shouldBe` Left (EvalFailure (LimitExceeded (StepLimit 2)))
    it "rejects when nothing is allowed" $
      run' defaultConfig{maxSteps = 0} "1" Null `shouldBe` Left (EvalFailure (LimitExceeded (StepLimit 0)))
    it "charges a lookup for the array elements it walks past" $ do
      let rule = "{\"var\":\"x.2\"}"
          dat = json "{\"x\":[10,20,30]}"
      -- the var and its path are two steps; walking past two elements is two more
      run' defaultConfig{maxSteps = 4} rule dat `shouldBe` Right (Number 30)
      run' defaultConfig{maxSteps = 3} rule dat `shouldSatisfy` isStepLimit
    it "charges every iteration of map" $ do
      let rule = "{\"map\":[[1,2,3],{\"var\":\"\"}]}"
      -- map node (1), the array and its three literals (4), and three
      -- bodies of two nodes each, the var and its path (6)
      run' defaultConfig{maxSteps = 11} rule Null `shouldBe` Right (toJSON [1, 2, 3 :: Int])
      run' defaultConfig{maxSteps = 10} rule Null `shouldSatisfy` isStepLimit

  describe "string length" $ do
    let rule = "{\"cat\":[\"aaaa\",\"bbbb\"]}"
    it "allows exactly the limit" $
      run' defaultConfig{maxStringLength = 8} rule Null `shouldBe` Right (String "aaaabbbb")
    it "rejects one over" $
      run' defaultConfig{maxStringLength = 7} rule Null
        `shouldBe` Left (EvalFailure (LimitExceeded (StringLimit 7)))
    it "counts UTF-16 code units, so an emoji is two" $ do
      run' defaultConfig{maxStringLength = 2} "{\"cat\":[\"\\ud83d\\ude00\"]}" Null `shouldBe` Right (String "\x1F600")
      run' defaultConfig{maxStringLength = 1} "{\"cat\":[\"\\ud83d\\ude00\"]}" Null
        `shouldBe` Left (EvalFailure (LimitExceeded (StringLimit 1)))

  describe "array length" $ do
    it "allows a merge exactly at the limit" $
      run' defaultConfig{maxArrayLength = 3} "{\"merge\":[[1,2],[3]]}" Null `shouldBe` Right (toJSON [1, 2, 3 :: Int])
    it "rejects a merge one over" $
      run' defaultConfig{maxArrayLength = 2} "{\"merge\":[[1,2],[3]]}" Null
        `shouldBe` Left (EvalFailure (LimitExceeded (ArrayLimit 2)))
    it "rejects mapping over an array longer than the limit, before any work" $
      run' defaultConfig{maxArrayLength = 2} "{\"map\":[[1,2,3],{\"var\":\"\"}]}" Null
        `shouldBe` Left (EvalFailure (LimitExceeded (ArrayLimit 2)))
    it "allows mapping over an array at the limit" $
      run' defaultConfig{maxArrayLength = 3} "{\"map\":[[1,2,3],{\"var\":\"\"}]}" Null `shouldBe` Right (toJSON [1, 2, 3 :: Int])
    it "rejects filtering an array longer than the limit" $
      run' defaultConfig{maxArrayLength = 2} "{\"filter\":[[1,2,3],true]}" Null
        `shouldBe` Left (EvalFailure (LimitExceeded (ArrayLimit 2)))

  describe "depth" $ do
    let nested n = foldr (\_ inner -> object ["!" .= inner]) (toJSON (0 :: Int)) [1 .. n :: Int]
        at limit n = applyWithConfig defaultConfig{maxDepth = limit} noOperators (nested n) Null
    it "accepts exactly the limit" $ at 5 5 `shouldSatisfy` isRight'
    it "rejects one past it" $ at 5 6 `shouldBe` Left (ParseFailure (DepthExceeded 5))
    it "counts the nesting of a literal object too" $ do
      let literal n = foldr (\_ inner -> object ["a" .= inner, "b" .= inner]) (toJSON (0 :: Int)) [1 .. n :: Int]
      applyWithConfig defaultConfig{maxDepth = 5} noOperators (literal 5) Null `shouldSatisfy` isRight'
      applyWithConfig defaultConfig{maxDepth = 5} noOperators (literal 6) Null
        `shouldBe` Left (ParseFailure (DepthExceeded 5))
    it "holds a custom operator's result to the same depth" $ do
      let deep n = foldr (\_ inner -> toJSON [inner]) (toJSON (0 :: Int)) [1 .. n :: Int]
          ops = either (error . show) id (operatorsFromList [(OperatorName "deep", \_ _ -> Right (deep 10))])
          call limit = applyWithConfig defaultConfig{maxDepth = limit} ops (object ["deep" .= ([] :: [Int])]) Null
      call 10 `shouldSatisfy` isRight'
      call 9 `shouldBe` Left (EvalFailure (LimitExceeded (ValueDepthLimit 9)))

  describe "amplification rules finish quickly and fail safely" $ do
    it "nested maps over a ten-element array (10^20 iterations)" $ do
      r <- within10s (applyWithConfig defaultConfig noOperators (nestedMap 20) Null)
      r `shouldSatisfy` isStepLimit
    it "a reduce that doubles a string on every step" $ do
      let rule =
            "{\"reduce\":[[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30],\
            \{\"cat\":[{\"var\":\"accumulator\"},{\"var\":\"accumulator\"}]},\"xy\"]}"
      r <- within10s (run' defaultConfig rule Null)
      -- building the string is paid for in steps, which run out first here
      r `shouldSatisfy` isStepLimit
      r' <- within10s (run' defaultConfig{maxSteps = 100000000} rule Null)
      r' `shouldBe` Left (EvalFailure (LimitExceeded (StringLimit 1048576)))
    it "a reduce that doubles an array on every step" $ do
      let rule =
            "{\"reduce\":[[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30],\
            \{\"merge\":[{\"var\":\"accumulator\"},{\"var\":\"accumulator\"}]},[1,2]]}"
      r <- within10s (run' defaultConfig rule Null)
      r `shouldBe` Left (EvalFailure (LimitExceeded (ArrayLimit 100000)))
    it "a very wide rule" $ do
      let wide = object ["+" .= replicate 2000000 (1 :: Int)]
      r <- within10s (applyWithConfig defaultConfig noOperators wide Null)
      r `shouldSatisfy` isStepLimit
    it "a deeply nested datum is only read, never recursed into by the evaluator" $ do
      let deepData = foldr (\_ inner -> toJSON [inner]) Null [1 .. 20000 :: Int]
      r <- within10s (applyWithConfig defaultConfig noOperators (object ["var" .= ("" :: String)]) deepData)
      r `shouldSatisfy` isRight'
    it "repeated lookups deep into a large array are paid for in steps" $ do
      let big = toJSON [1 .. 5000 :: Int]
          rule = object ["+" .= replicate 2000 (object ["var" .= ("x.4999" :: String)])]
      r <- within10s (applyWithConfig defaultConfig noOperators rule (object ["x" .= big]))
      r `shouldSatisfy` isStepLimit
    it "repeated comparison of a large array is paid for in steps" $ do
      let big = toJSON [1 .. 5000 :: Int]
          rule = object ["+" .= replicate 2000 (object ["==" .= [object ["var" .= ("x" :: String)], toJSON (1 :: Int)]])]
      r <- within10s (applyWithConfig defaultConfig noOperators rule (object ["x" .= big]))
      r `shouldSatisfy` isStepLimit

  describe "properties" $ do
    it "evaluation always terminates, within a constant factor of the budget" $
      forAll (genRule 6) $ \rule -> forAll (genData 3) $ \dat ->
        forAll (choose (0, 60)) $ \budget ->
          ioProperty $ do
            r <- within10s (applyWithConfig defaultConfig{maxSteps = budget, arityMode = Lenient} noOperators rule dat)
            pure (r `seq` True)
    it "raising the step limit never changes an answer that already succeeded" $
      forAll (genRule 5) $ \rule -> forAll (genData 3) $ \dat ->
        forAll (choose (1, 80)) $ \budget ->
          let low = applyWithConfig defaultConfig{maxSteps = budget, arityMode = Lenient} noOperators rule dat
              high = applyWithConfig defaultConfig{maxSteps = budget * 100, arityMode = Lenient} noOperators rule dat
           in case low of
                Right v -> high === Right v
                Left (EvalFailure (LimitExceeded _)) -> property True
                Left e -> high === Left e
    it "compile once then run equals apply" $
      forAll (genRule 5) $ \rule -> forAll (genData 3) $ \dat ->
        let direct = apply rule dat
            staged = compile defaultConfig rule >>= \c -> run noOperators c dat
         in direct === staged

isRight' :: Either a b -> Bool
isRight' = either (const False) (const True)

isStepLimit :: Either Error Value -> Bool
isStepLimit (Left (EvalFailure (LimitExceeded (StepLimit _)))) = True
isStepLimit _ = False
