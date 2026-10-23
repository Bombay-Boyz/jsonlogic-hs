{-# LANGUAGE OverloadedStrings #-}

-- | Error locations are JSON Pointers into the rule: every one of them must
-- name a value that exists in the rule's JSON, and that value must be the
-- operator call (or literal) the error is about.
module LocationSpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Foldable (toList)
import Data.Maybe (isJust)
import Test.Hspec
import Test.QuickCheck

import Gen (genData, genRule)
import JsonLogic
import Support (at, json)

-- | The value a pointer names, if it names one (RFC 6901 resolution, on
-- already-decoded segments).
resolve :: Location -> Value -> Maybe Value
resolve location = go (locationSegments location)
  where
    go [] v = Just v
    go (Key k : rest) (Object fields) = KeyMap.lookup (Key.fromText k) fields >>= go rest
    go (Index i : rest) (Array items) = case drop i (toList items) of
      item : _ | i >= 0 -> go rest item
      _ -> Nothing
    go _ _ = Nothing

-- | For an operator error, the operator call the pointer names.
operatorAt :: Location -> Value -> Maybe OperatorName
operatorAt location rule = case resolve location rule of
  Just (Object fields) | [(key, _)] <- KeyMap.toList fields -> Just (OperatorName (Key.toText key))
  _ -> Nothing

-- | The operator an error is about, when it is about one.
operatorOf :: Error -> Maybe OperatorName
operatorOf e = case e of
  ParseFailure _ (ArityMismatch name _ _) -> Just name
  EvalFailure _ (UnknownOperator name) -> Just name
  EvalFailure _ (CustomOperatorFailed name _) -> Just name
  EvalFailure _ (InvalidOperand name) -> Just name
  _ -> Nothing

modes :: [Config]
modes = [defaultConfig, defaultConfig{arityMode = Lenient}, defaultConfig{maxSteps = 25}, defaultConfig{maxDepth = 3}]

spec :: Spec
spec = describe "error locations" $ do
  it "always name a value that exists in the rule" $
    forAll (genRule 6) $ \rule -> forAll (genData 3) $ \dat -> forAll (elements modes) $ \config ->
      case applyWithConfig config noOperators rule dat of
        Left e -> counterexample (show e) (isJust (resolve (errorLocation e) rule))
        Right _ -> property True
  it "name the very operator an operator error is about" $
    forAll (genRule 6) $ \rule -> forAll (genData 3) $ \dat -> forAll (elements modes) $ \config ->
      case applyWithConfig config noOperators rule dat of
        Left e | Just name <- operatorOf e -> counterexample (show e) (operatorAt (errorLocation e) rule === Just name)
        _ -> property True
  it "follow bare operands, which have no index" $
    apply (json "{\"!\":{\"!\":{\"nope\":1}}}") Null
      `shouldBe` Left (EvalFailure (at [Left "!", Left "!"]) (UnknownOperator (OperatorName "nope")))
  it "point into literals' own arrays" $
    apply (json "{\"merge\":[[{\"nope\":[]}]]}") Null
      `shouldBe` Left (EvalFailure (at [Left "merge", Right 0, Right 0]) (UnknownOperator (OperatorName "nope")))
  it "put an operand the rule leaves out at the operator call, which exists" $
    -- five steps: the and, its true, the ==, its 1, and the undefined that
    -- stands in for the missing second operand, which runs out
    applyWithConfig defaultConfig{arityMode = Lenient, maxSteps = 4} noOperators (json "{\"and\":[true,{\"==\":[1]}]}") Null
      `shouldBe` Left (EvalFailure (at [Left "and", Right 1]) (LimitExceeded (StepLimit 4)))
