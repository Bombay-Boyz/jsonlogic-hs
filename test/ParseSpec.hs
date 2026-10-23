{-# LANGUAGE OverloadedStrings #-}

module ParseSpec (spec) where

import Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.Aeson.Key as Key
import Data.Either (isRight)
import Data.Fix (Fix (..), foldFix)
import Data.List (nub)
import Data.Text (Text)
import Test.Hspec
import Test.QuickCheck (NonNegative (..), forAll, property)

import Gen (genRule)
import JsonLogic.Core.Config (ArityMode (..), Config (..), defaultConfig)
import JsonLogic.Core.Errors (Arity (..), ParseError (..))
import JsonLogic.Core.Location (Location, root)
import JsonLogic.Core.Operator (OperatorName (..), allBuiltins, builtinName, lookupBuiltin)
import JsonLogic.Core.Parse (parseRule, parseRuleWith)
import JsonLogic.Core.Rule (Node (..), Rule, RuleF (..), Term, stripLocations)
import Support (at, json)

lit :: Value -> Term
lit = Fix . Lit

int :: Int -> Value
int = toJSON

-- | A rule nested @n@ operator levels deep around a literal leaf.
nested :: Int -> Value
nested n = foldr (\_ inner -> object ["!" .= inner]) (int 0) [1 .. n]

-- | The shape a rule is read as, without locations.
shape :: Value -> Either ParseError Term
shape = either (Left . snd) (Right . stripLocations) . parseRule

lenientShape :: Value -> Either ParseError Term
lenientShape = either (Left . snd) (Right . stripLocations) . parseRuleWith defaultConfig{arityMode = Lenient}

-- | The location of every node, outermost first.
locations :: Rule -> [Location]
locations = foldFix (\(Node location node) -> location : concat node)

parsedLocations :: Value -> [Location]
parsedLocations = either (const []) locations . parseRule

withDepth :: Int -> Value -> Either (Location, ParseError) Rule
withDepth limit = parseRuleWith defaultConfig{maxDepth = limit}

spec :: Spec
spec = describe "parsing" $ do
  describe "shapes" $ do
    it "reads a bare number as a literal" $
      shape (int 1) `shouldBe` Right (lit (int 1))
    it "reads {\"var\": \"a\"} as a var with no default" $
      shape (object ["var" .= ("a" :: Text)])
        `shouldBe` Right (Fix (Var (lit (String "a")) Nothing))
    it "reads {\"var\": [\"a\", 1]} as a var with a default" $
      shape (object ["var" .= [toJSON ("a" :: Text), int 1]])
        `shouldBe` Right (Fix (Var (lit (String "a")) (Just (lit (int 1)))))
    it "reads {\"var\": []} as a lookup of the data itself" $
      shape (object ["var" .= ([] :: [Int])])
        `shouldBe` Right (Fix (Var (lit Null) Nothing))
    it "reads an array element-wise, as the reference implementation does" $
      shape (toJSON [1, 2 :: Int]) `shouldBe` Right (Fix (Arr [lit (int 1), lit (int 2)]))
    it "reads an empty-argument + as Add []" $
      shape (object ["+" .= ([] :: [Int])]) `shouldBe` Right (Fix (Add []))
    it "treats an object with two keys as a literal, not a rule" $ do
      let v = object ["a" .= int 1, "b" .= int 2]
      shape v `shouldBe` Right (lit v)
    it "turns an unknown operator into a CustomOp" $
      shape (object ["frobnicate" .= ([] :: [Int])])
        `shouldBe` Right (Fix (CustomOp (OperatorName "frobnicate") []))
    it "reads a bare operand as a list of one" $
      shape (object ["!" .= True]) `shouldBe` shape (object ["!" .= [True]])

  describe "strict arity" $ do
    it "rejects a wrong argument count" $
      shape (object ["==" .= [int 1]]) `shouldBe` Left (ArityMismatch (OperatorName "==") (Exactly 2) 1)
    it "accepts the three-argument between form for < but not for >" $ do
      shape (object ["<" .= [int 1, int 2, int 3]]) `shouldSatisfy` isRight
      shape (object [">" .= [int 1, int 2, int 3]])
        `shouldBe` Left (ArityMismatch (OperatorName ">") (Exactly 2) 3)
    it "states var's range as zero to two" $
      shape (object ["var" .= [int 1, int 2, int 3]])
        `shouldBe` Left (ArityMismatch (OperatorName "var") (Between 0 2) 3)
    it "rejects * with no operands" $
      shape (object ["*" .= ([] :: [Int])]) `shouldBe` Left (ArityMismatch (OperatorName "*") (AtLeast 1) 0)

  describe "lenient arity" $ do
    it "pads a missing operand with undefined (an empty and)" $
      lenientShape (object ["==" .= [int 1]])
        `shouldBe` Right (Fix (LooseEq (lit (int 1)) (Fix (And []))))
    it "pads the missing initial value of reduce with null" $
      lenientShape (object ["reduce" .= [String "xs", int 2]])
        `shouldBe` Right (Fix (Reduce (lit (String "xs")) (lit (int 2)) (lit Null)))
    it "keeps surplus operands of an ordinary operator to be evaluated" $
      lenientShape (object ["!" .= [int 1, int 2]])
        `shouldBe` Right (Fix (Discard (Fix (Not (lit (int 1)))) [lit (int 2)]))
    it "drops surplus operands of map, which the reference never looks at" $
      lenientShape (object ["map" .= [int 1, int 2, int 3]])
        `shouldBe` Right (Fix (Map (lit (int 1)) (lit (int 2))))
    it "keeps an empty *, which throws only if it is evaluated" $
      lenientShape (object ["*" .= ([] :: [Int])]) `shouldBe` Right (Fix (Mul []))
    it "never reports an arity error" $
      forAll (genRule 5) $ \rule -> case lenientShape rule of
        Left (ArityMismatch{}) -> False
        _ -> True

  describe "locations" $ do
    it "puts the root at the empty pointer" $
      take 1 (parsedLocations (int 1)) `shouldBe` [root]
    it "locates operands under the operator's key, by index" $
      parsedLocations (json "{\"if\":[true,{\"var\":\"a\"}]}")
        `shouldBe` [ root
                   , at [Left "if", Right 0]
                   , at [Left "if", Right 1]
                   , at [Left "if", Right 1, Left "var"]
                   ]
    it "locates a bare operand at the operator's key" $
      parsedLocations (json "{\"!\":{\"var\":\"a\"}}")
        `shouldBe` [root, at [Left "!"], at [Left "!", Left "var"]]
    it "locates array elements by index" $
      parsedLocations (json "[1,[2]]")
        `shouldBe` [root, at [Right 0], at [Right 1], at [Right 1, Right 0]]
    it "reports where an arity error is" $
      parseRule (json "{\"and\":[true,{\"==\":[1]}]}")
        `shouldSatisfy` either ((== at [Left "and", Right 1]) . fst) (const False)
    it "reports where the depth limit was passed" $
      withDepth 1 (json "{\"!\":{\"!\":{\"!\":1}}}")
        `shouldSatisfy` either ((== at [Left "!", Left "!"]) . fst) (const False)

  describe "depth" $ do
    it "reports the configured limit when nesting is too deep" $
      either (Left . snd) (const (Right ())) (withDepth 3 (nested 10)) `shouldBe` Left (DepthExceeded 3)
    it "accepts nesting exactly up to the depth limit and no deeper" $
      property $ \(NonNegative n) (NonNegative limit) ->
        isRight (withDepth limit (nested n)) == (n <= limit)

  describe "the operator table" $ do
    it "maps every builtin's name back to it" $
      map (lookupBuiltin . builtinName) allBuiltins `shouldBe` map Just allBuiltins
    it "has the 35 builtins of the classic specification, with distinct names" $
      length (nub (map builtinName allBuiltins)) `shouldBe` 35
    it "reads every builtin name as a builtin, never as a custom operator" $
      mapM_
        ( \builtin -> case lenientShape (object [Key.fromText (builtinName builtin) .= ([] :: [Int])]) of
            Right (Fix (CustomOp _ _)) -> expectationFailure ("read as custom: " ++ show builtin)
            _ -> pure ()
        )
        allBuiltins
