{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (Value (..), object, toJSON, (.=))
import Data.Either (isRight)
import Data.Fix (Fix (..))
import Data.Text (Text)
import Test.Hspec
import Test.QuickCheck (NonNegative (..), property)

import JsonLogic.Core.Errors (Arity (..), ParseError (..))
import JsonLogic.Core.Parse (parseRule, parseRuleWithDepth)
import JsonLogic.Core.Rule (OperatorName (..), Rule, RuleF (..))

lit :: Value -> Rule
lit = Fix . Lit

int :: Int -> Value
int = toJSON

-- | A rule nested @n@ operator levels deep around a literal leaf.
nested :: Int -> Value
nested n = foldr (\_ inner -> object ["!" .= inner]) (int 0) [1 .. n]

main :: IO ()
main = hspec $
  describe "parseRule" $ do
    it "reads a bare number as a literal" $
      parseRule (int 1) `shouldBe` Right (lit (int 1))

    it "reads {\"var\": \"a\"} as a var with no default" $
      parseRule (object ["var" .= ("a" :: Text)])
        `shouldBe` Right (Fix (Var (lit (String "a")) Nothing))

    it "reads {\"var\": [\"a\", 1]} as a var with a default" $
      parseRule (object ["var" .= [toJSON ("a" :: Text), int 1]])
        `shouldBe` Right (Fix (Var (lit (String "a")) (Just (lit (int 1)))))

    it "reads an array element-wise, as the reference implementation does" $
      parseRule (toJSON [1, 2 :: Int])
        `shouldBe` Right (Fix (Arr [lit (int 1), lit (int 2)]))

    it "reads an empty-argument + as Add []" $
      parseRule (object ["+" .= ([] :: [Int])]) `shouldBe` Right (Fix (Add []))

    it "treats an object with two keys as a literal, not a rule" $ do
      let v = object ["a" .= int 1, "b" .= int 2]
      parseRule v `shouldBe` Right (lit v)

    it "turns an unknown operator into a CustomOp" $
      parseRule (object ["frobnicate" .= ([] :: [Int])])
        `shouldBe` Right (Fix (CustomOp (OperatorName "frobnicate") []))

    it "rejects a wrong argument count" $
      parseRule (object ["==" .= [int 1]])
        `shouldBe` Left (ArityMismatch (OperatorName "==") (Exactly 2) 1)

    it "accepts the three-argument between form for < but not for >" $ do
      parseRule (object ["<" .= [int 1, int 2, int 3]]) `shouldSatisfy` isRight
      parseRule (object [">" .= [int 1, int 2, int 3]])
        `shouldBe` Left (ArityMismatch (OperatorName ">") (Exactly 2) 3)

    it "reports the configured limit when nesting is too deep" $
      parseRuleWithDepth 3 (nested 10) `shouldBe` Left (DepthExceeded 3)

    it "accepts nesting exactly up to the depth limit and no deeper" $
      property $ \(NonNegative n) (NonNegative limit) ->
        isRight (parseRuleWithDepth limit (nested n)) == (n <= limit)
