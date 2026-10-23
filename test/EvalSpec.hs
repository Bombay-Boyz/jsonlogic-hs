{-# LANGUAGE OverloadedStrings #-}

module EvalSpec (spec) where

import Data.Aeson (Value (..), decode, object, toJSON, (.=))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.Fix (Fix (..))
import qualified Data.Text as Text
import Test.Hspec
import Test.QuickCheck (Gen, choose, forAll, listOf, oneof)

import JsonLogic hiding (Rule)
import JsonLogic.Core.Eval (evaluate)
import JsonLogic.Core.Location (root)
import JsonLogic.Core.Rule (Node (..), Rule, RuleF (..))
import Support (Problem (..), at, problem)

-- | Test cases are written with single quotes so the JSON stays readable.
q :: LBS.ByteString -> LBS.ByteString
q = LBS.map (\byte -> if byte == 39 then 34 else byte)

-- | @evaluates rule input expected@
evaluates :: LBS.ByteString -> LBS.ByteString -> LBS.ByteString -> Expectation
evaluates rule input expected =
  case (decode (q rule), decode (q input), decode (q expected)) of
    (Just r, Just i, Just e) -> apply r i `shouldBe` Right e
    _ -> expectationFailure "test case contains invalid JSON"

-- | (rule, data, expected result). Every one of these is behaviour of the
-- reference implementation, including its quirks.
cases :: [(LBS.ByteString, LBS.ByteString, LBS.ByteString)]
cases =
  [ -- equality and logic
    ("{'==':[1,1]}", "{}", "true")
  , ("{'==':[1,'1']}", "{}", "true")
  , ("{'===':[1,'1']}", "{}", "false")
  , ("{'!=':[1,2]}", "{}", "true")
  , ("{'!==':[1,'1']}", "{}", "true")
  , ("{'!':[true]}", "{}", "false")
  , ("{'!!':[[]]}", "{}", "false")
  , ("{'!!':['0']}", "{}", "true")
  , ("{'and':[true,'a',3]}", "{}", "3")
  , ("{'and':[true,'',3]}", "{}", "''")
  , ("{'or':[false,'',0]}", "{}", "0")
  , ("{'or':[false,'a']}", "{}", "'a'")
  , ("{'and':[]}", "{}", "null")
  , -- if
    ("{'if':[true,'yes','no']}", "{}", "'yes'")
  , ("{'if':[false,'a',true,'b','c']}", "{}", "'b'")
  , ("{'if':[false,'a','c']}", "{}", "'c'")
  , ("{'if':[]}", "{}", "null")
  , -- variables
    ("{'var':'a'}", "{'a':1}", "1")
  , ("{'var':['z',26]}", "{'a':1}", "26")
  , ("{'var':'a.b'}", "{'a':{'b':2}}", "2")
  , ("{'var':1}", "['apple','banana']", "'banana'")
  , ("{'var':''}", "{'a':1}", "{'a':1}")
  , ("{'var':'a.length'}", "{'a':[1,2,3]}", "3")
  , ("{'var':['a',5]}", "{'a':null}", "null")
  , ("{'missing':['a','b']}", "{'a':1}", "['b']")
  , ("{'missing_some':[1,['a','b','c']]}", "{'a':1}", "[]")
  , ("{'missing_some':[2,['a','b','c']]}", "{'a':1}", "['b','c']")
  , -- ordering
    ("{'<':[1,2]}", "{}", "true")
  , ("{'<':[1,2,3]}", "{}", "true")
  , ("{'<':[1,3,2]}", "{}", "false")
  , ("{'<=':[1,1]}", "{}", "true")
  , ("{'>':[2,1]}", "{}", "true")
  , ("{'>=':[1,1]}", "{}", "true")
  , ("{'<':['10','9']}", "{}", "true")
  , ("{'<':['2',10]}", "{}", "true")
  , -- arithmetic
    ("{'+':[1,2,3]}", "{}", "6")
  , ("{'+':['3.14']}", "{}", "3.14")
  , ("{'+':[]}", "{}", "0")
  , ("{'*':[2,3,4]}", "{}", "24")
  , ("{'*':['3']}", "{}", "'3'")
  , ("{'-':[5,2]}", "{}", "3")
  , ("{'-':[5]}", "{}", "-5")
  , ("{'/':[4,2]}", "{}", "2")
  , ("{'/':[1,0]}", "{}", "null")
  , ("{'%':[101,2]}", "{}", "1")
  , ("{'max':[1,3,2]}", "{}", "3")
  , ("{'min':[]}", "{}", "null")
  , ("{'max':[1,'a']}", "{}", "null")
  , -- collections
    ("{'map':[{'var':'nums'},{'*':[{'var':''},2]}]}", "{'nums':[1,2,3]}", "[2,4,6]")
  , ("{'filter':[[1,2,3,4],{'%':[{'var':''},2]}]}", "{}", "[1,3]")
  , ("{'reduce':[[1,2,3],{'+':[{'var':'current'},{'var':'accumulator'}]},0]}", "{}", "6")
  , ("{'all':[[1,2],{'>':[{'var':''},0]}]}", "{}", "true")
  , ("{'all':[[],true]}", "{}", "false")
  , ("{'none':[[1,2],{'<':[{'var':''},0]}]}", "{}", "true")
  , ("{'some':[[1,-2],{'<':[{'var':''},0]}]}", "{}", "true")
  , ("{'map':[5,{'var':''}]}", "{}", "[]")
  , ("{'merge':[[1,2],[3,[4]],5]}", "{}", "[1,2,3,[4],5]")
  , -- strings
    ("{'in':['Spring','Springfield']}", "{}", "true")
  , ("{'in':['a',['a','b']]}", "{}", "true")
  , ("{'in':['z',['a']]}", "{}", "false")
  , ("{'cat':['a',1,null,true]}", "{}", "'a1true'")
  , ("{'substr':['jsonlogic',4]}", "{}", "'logic'")
  , ("{'substr':['jsonlogic',-5]}", "{}", "'logic'")
  , ("{'substr':['jsonlogic',1,3]}", "{}", "'son'")
  , ("{'substr':['jsonlogic',4,-2]}", "{}", "'log'")
  , ("{'log':'hi'}", "{}", "'hi'")
  , -- undefined (what an empty and / or yields) is not null
    ("{'max':[{'and':[]}]}", "{}", "null")
  , ("{'+':[1,{'and':[]}]}", "{}", "null")
  , ("{'cat':['a',{'and':[]},'b']}", "{}", "'ab'")
  , ("{'==':[{'and':[]},null]}", "{}", "true")
  , ("{'===':[{'and':[]},null]}", "{}", "false")
  , ("{'==':[{'and':[]},0]}", "{}", "false")
  , ("{'==':[{'and':[]},false]}", "{}", "false")
  , ("{'in':[{'and':[]},'xundefinedx']}", "{}", "true")
  , ("{'substr':[{'and':[]},0,3]}", "{}", "'und'")
  , ("{'<':[{'and':[]},1]}", "{}", "false")
  , ("{'min':[{'>=':[true,{'and':[]}]}]}", "{}", "0")
  , ("{'or':[false,{'and':[]}]}", "{}", "null")
  , ("{'!':[{'and':[]}]}", "{}", "true")
  , ("{'-':[{'and':[]}]}", "{}", "null")
  , ("{'var':[{'and':[]}]}", "{'a':1}", "{'a':1}")
  , ("{'var':['a',{'and':[]}]}", "{}", "null")
  , ("{'map':[[{'and':[]},1],{'var':''}]}", "{}", "[null,1]")
  , ("{'map':[[{'and':[]},1],{'var':'x'}]}", "{}", "[null,null]")
  , ("{'merge':[[{'and':[]}]]}", "{}", "[null]")
  , ("{'missing':[{'and':[]}]}", "{'a':1}", "[]")
  , -- an optional operand that evaluates to undefined counts as absent
    ("{'-':[5,{'and':[]}]}", "{}", "-5")
  , ("{'-':['7',{'and':[]}]}", "{}", "-7")
  , ("{'-':[[1.5],{'and':[]}]}", "{}", "-1.5")
  , ("{'-':[[],{'and':[]}]}", "{}", "0")
  , ("{'<':[1,2,{'and':[]}]}", "{}", "true")
  , ("{'<':[2,1,{'and':[]}]}", "{}", "false")
  , ("{'<=':[1,1,{'and':[]}]}", "{}", "true")
  , ("{'substr':['jsonlogic',4,{'and':[]}]}", "{}", "'logic'")
  , ("{'substr':['jsonlogic',4,null]}", "{}", "''")
  , ("{'substr':['jsonlogic',{'and':[]},3]}", "{}", "'jso'")
  , -- numbers beyond 2^53 print and parse exactly as JavaScript does
    ("{'max':[{'cat':[-2.25,'3',1e21]}]}", "{}", "-2.2531e21")
  , ("{'cat':[2.2531e21]}", "{}", "'2.2531e+21'")
  , ("{'cat':[9007199254740993]}", "{}", "'9007199254740992'")
  , ("{'cat':[1e21]}", "{}", "'1e+21'")
  , ("{'+':['1e400']}", "{}", "null")
  , ("{'+':['1e-400']}", "{}", "0")
  , ("{'+':['5e-324']}", "{}", "5e-324")
  , ("{'+':['123456789012345678901234567890']}", "{}", "1.2345678901234568e29")
  , -- literals, arrays, laziness
    ("[{'var':'a'},2]", "{'a':1}", "[1,2]")
  , ("{'a':1,'b':2}", "{}", "{'a':1,'b':2}")
  , ("5", "{}", "5")
  , ("{'or':[true,{'nope':[]}]}", "{}", "true")
  , ("{'and':[false,{'nope':[]}]}", "{}", "false")
  , ("{'if':[true,'a',{'nope':[]}]}", "{}", "'a'")
  ]

label :: LBS.ByteString -> LBS.ByteString -> String
label rule input = Char8.unpack (q rule) ++ "  on  " ++ Char8.unpack (q input)

-- | A rule nested @n@ operator levels deep around a literal leaf.
nested :: Int -> Value
nested n = foldr (\_ inner -> object ["!" .= inner]) (Number 0) [1 .. n]

doubling :: Operators
doubling = either (error . show) id (operatorsFromList [(OperatorName "double", implementation)])
  where
    implementation _ [Number n] = Right (Number (n * 2))
    implementation _ _ = Left "expected one number"

withJson :: LBS.ByteString -> (Value -> Expectation) -> Expectation
withJson text check =
  maybe (expectationFailure "invalid JSON in test") check (decode (q text))

genJson :: Gen Value
genJson = oneof [scalar, toJSON <$> listOf scalar]
  where
    scalar =
      oneof
        [ pure Null
        , Bool <$> oneof [pure True, pure False]
        , toJSON <$> (choose (-1000, 1000) :: Gen Int)
        , String . Text.pack <$> listOf (choose ('a', 'z'))
        ]

-- | A node built by hand, at the root (the laws do not look at locations).
node :: RuleF Rule -> Rule
node = Fix . Node root

lit :: Value -> Rule
lit = node . Lit

result :: Rule -> Either EvalError Value
result rule = either (Left . snd) Right (evaluate rule Null)

spec :: Spec
spec = describe "evaluation" $ do
  describe "reference behaviour" $
    mapM_ (\(rule, input, expected) -> it (label rule input) (evaluates rule input expected)) cases

  describe "errors" $ do
    it "reports an unknown operator, where it is" $
      withJson "{'nope':[1]}" $ \rule ->
        apply rule Null `shouldBe` Left (EvalFailure root (UnknownOperator (OperatorName "nope")))
    it "evaluates arguments before the operator, so a bad one still fails" $
      withJson "{'+':[1,{'nope':[]}]}" $ \rule ->
        apply rule Null `shouldBe` Left (EvalFailure (at [Left "+", Right 1]) (UnknownOperator (OperatorName "nope")))
    it "locates a failure inside an iteration body" $
      withJson "{'map':[[1],{'nope':[]}]}" $ \rule ->
        apply rule Null `shouldBe` Left (EvalFailure (at [Left "map", Right 1]) (UnknownOperator (OperatorName "nope")))
    it "surfaces parse errors, with their location" $
      withJson "{'!':{'==':[1]}}" $ \rule ->
        apply rule Null
          `shouldBe` Left (ParseFailure (at [Left "!"]) (ArityMismatch (OperatorName "==") (Exactly 2) 1))
    it "rejects rules nested past the depth limit before evaluating" $
      problem (apply (nested 65) Null) `shouldBe` Left (ParseProblem (DepthExceeded 64))
    it "renders an error with its JSON Pointer" $ do
      renderError (EvalFailure (at [Left "if", Right 1]) (UnknownOperator (OperatorName "x")))
        `shouldBe` "at /if/1: unknown operator \"x\""
      renderError (EvalFailure root (UnknownOperator (OperatorName "x")))
        `shouldBe` "at the root of the rule: unknown operator \"x\""
    it "escapes ~ and / in a pointer, as RFC 6901 requires" $
      renderLocation (at [Left "a/b", Left "c~d", Right 3]) `shouldBe` "/a~1b/c~0d/3"

  describe "custom operators" $ do
    it "runs a supplied operator on evaluated arguments" $
      withJson "{'double':[{'var':'x'}]}" $ \rule ->
        withJson "{'x':21}" $ \input ->
          applyWith doubling rule input `shouldBe` Right (Number 42)
    it "reports the operator's own failure" $
      withJson "{'double':['a']}" $ \rule ->
        applyWith doubling rule Null
          `shouldBe` Left (EvalFailure root (CustomOperatorFailed (OperatorName "double") "expected one number"))

  describe "laws" $ do
    it "De Morgan: not (x and y) agrees with (not x) or (not y)" $
      forAll genJson $ \x -> forAll genJson $ \y ->
        result (node (Not (node (And [lit x, lit y]))))
          == result (node (Or [node (Not (lit x)), node (Not (lit y))]))
    it "!! is not-not" $
      forAll genJson $ \x ->
        result (node (DoubleNot (lit x))) == result (node (Not (node (Not (lit x)))))
    it "!! is idempotent" $
      forAll genJson $ \x ->
        result (node (DoubleNot (node (DoubleNot (lit x))))) == result (node (DoubleNot (lit x)))
    it "if with a true condition is its consequent" $
      forAll genJson $ \a -> forAll genJson $ \b ->
        result (node (If [lit (Bool True), lit a, lit b])) == result (lit a)
    it "if with a false condition is its alternative" $
      forAll genJson $ \a -> forAll genJson $ \b ->
        result (node (If [lit (Bool False), lit a, lit b])) == result (lit b)
    it "a one-operand and / or is that operand" $
      forAll genJson $ \x ->
        result (node (And [lit x])) == result (lit x)
          && result (node (Or [lit x])) == result (lit x)
