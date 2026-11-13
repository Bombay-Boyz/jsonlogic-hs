{-# LANGUAGE OverloadedStrings #-}

-- | Behaviour that a forensic engineering audit found wrong, pinned so it
-- stays fixed. Each group names the finding (F-nn) it covers.
module AuditSpec (spec) where

import Control.Concurrent (forkIO, killThread, newEmptyMVar, putMVar, takeMVar, threadDelay)
import qualified Control.Exception
import Control.Exception (AsyncException (..), Exception, fromException, throw, try)
import Control.Monad (forM, forM_)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import Data.Either (isLeft, isRight)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import System.Timeout (timeout)
import Test.Hspec
import Test.QuickCheck

import Gen (genData, genRule)
import JsonLogic
import JsonLogic.Core.Value (firstLossyNumber)
import Support (at, json)

-- ---------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------

call :: Text -> [Value] -> Value
call operator operands = object [Key.fromText operator .= operands]

registry :: [(Text, CustomOperator)] -> Operators
registry entries =
  either (error . show) id (operatorsFromList [(OperatorName name, implementation) | (name, implementation) <- entries])

anyArity :: (Value -> [Value] -> Either CustomError Value) -> CustomOperator
anyArity = pureOperator (AtLeast 0)

lenientConfig, lazyConfig, preserve, rejectLossy :: Config
lenientConfig = defaultConfig {arityMode = Lenient}
lazyConfig = defaultConfig {unknownOperators = FailWhenReached}
preserve = defaultConfig {numericSafety = PreserveSource}
rejectLossy = defaultConfig {numericSafety = RejectLossy}

withRule :: Operators -> Value -> (Rule -> IO ()) -> IO ()
withRule operators rule test = case compile defaultConfig operators rule of
  Left e -> expectationFailure ("did not compile: " ++ Text.unpack (renderError e))
  Right ready -> test ready

spinner :: Operators
spinner = registry [("spin", anyArity (\_ _ -> seq (length [1 :: Integer ..]) (Right Null)))]

data Boom = Boom
  deriving (Show)

instance Exception Boom

-- | The result of a rule as a value, with the error reduced to its kind.
kind :: Either Error Value -> Either Text Value
kind = either (Left . errorKind) Right

spec :: Spec
spec = describe "audit findings" $ do
  f01
  f02
  f03
  f04
  f05
  f06
  f07
  f10
  f11
  f13
  f15
  concurrency

-- ---------------------------------------------------------------------
-- F-01: tryRun and asynchronous exceptions
-- ---------------------------------------------------------------------

f01 :: Spec
f01 = describe "F-01 tryRun never swallows an asynchronous exception" $ do
  it "a timeout around tryRun fires, instead of becoming a Left" $
    withRule spinner (call "spin" []) $ \ready -> do
      outcome <- timeout 100000 (tryRun ready Null)
      isNothing outcome `shouldBe` True
  it "killThread reaches the thread running tryRun" $
    withRule spinner (call "spin" []) $ \ready -> do
      done <- newEmptyMVar
      thread <- forkIO (try (tryRun ready Null) >>= putMVar done)
      threadDelay 50000
      killThread thread
      result <- timeout 3000000 (takeMVar done)
      case result of
        Just (Left e) -> fromException e `shouldBe` Just ThreadKilled
        Just (Right _) -> expectationFailure "tryRun swallowed ThreadKilled and finished normally"
        Nothing -> expectationFailure "the killed thread never finished"
  it "runWithTimeout reports TimedOut" $
    withRule spinner (call "spin" []) $ \ready -> do
      outcome <- runWithTimeout 100000 ready Null
      case outcome of
        Left TimedOut -> pure ()
        other -> expectationFailure ("expected TimedOut, got " ++ show other)
  it "a synchronous exception from an operator is still reported, as OperatorThrew" $
    withRule (registry [("boom", anyArity (\_ _ -> throw Boom))]) (call "boom" []) $ \ready -> do
      outcome <- tryRun ready Null
      case outcome of
        Left (OperatorThrew e) -> fromException e `shouldSatisfy` \b -> case b of Just Boom -> True; Nothing -> False
        other -> expectationFailure ("expected OperatorThrew, got " ++ show other)
  it "a rejection stays a value, apart from the exceptions" $
    withRule (registry [("fail", anyArity (\_ _ -> Left "no"))]) (call "fail" []) $ \ready -> do
      outcome <- tryRun ready Null
      case outcome of
        Left (Rejected (EvalFailure _ (CustomOperatorFailed _ _))) -> pure ()
        other -> expectationFailure ("expected Rejected, got " ++ show other)
  it "an ordinary answer is returned unchanged" $
    withRule noOperators (call "+" [Number 1, Number 2]) $ \ready -> do
      outcome <- tryRun ready Null
      either (const Nothing) Just outcome `shouldBe` Just (Number 3)

-- ---------------------------------------------------------------------
-- F-02: unknown operators
-- ---------------------------------------------------------------------

f02 :: Spec
f02 = describe "F-02 unknown operators are found when the rule is compiled" $ do
  let typo = call "if" [Bool True, call "nope" [Number 1]]
  it "are refused by default, with their location" $
    case compile defaultConfig noOperators typo of
      Left (ParseFailure loc (UnknownOperatorName name _)) -> do
        loc `shouldBe` at [Left "if", Right 1]
        name `shouldBe` OperatorName "nope"
      other -> expectationFailure ("expected UnknownOperatorName, got " ++ either show (const "a rule") other)
  it "name the nearest known operators" $
    case compile defaultConfig noOperators (call "missing_sone" []) of
      Left (ParseFailure _ (UnknownOperatorName _ near)) -> near `shouldBe` [OperatorName "missing_some"]
      other -> expectationFailure ("expected a suggestion, got " ++ either show (const "a rule") other)
  it "suggest registered operators too" $
    case compile defaultConfig (registry [("starts_with", anyArity (\_ _ -> Right Null))]) (call "start_with" []) of
      Left (ParseFailure _ (UnknownOperatorName _ near)) -> near `shouldBe` [OperatorName "starts_with"]
      other -> expectationFailure ("expected a suggestion, got " ++ either show (const "a rule") other)
  it "suggest nothing when nothing is close" $
    case compile defaultConfig noOperators (call "xyzzy" []) of
      Left (ParseFailure _ (UnknownOperatorName _ near)) -> near `shouldBe` []
      other -> expectationFailure ("expected no suggestion, got " ++ either show (const "a rule") other)
  it "are accepted when the configuration says they fail only when reached, as the reference does" $ do
    apply typo Null `shouldSatisfy` isLeft
    applyWithConfig lazyConfig noOperators (call "or" [Bool True, call "nope" []]) Null `shouldBe` Right (Bool True)
    applyWithConfig lazyConfig noOperators typo Null
      `shouldBe` Left (EvalFailure (at [Left "if", Right 1]) (UnknownOperator (OperatorName "nope")))
  it "follow the arity mode by default: lenient mode keeps the reference's laziness" $
    applyWithConfig lenientConfig noOperators (call "or" [Bool True, call "nope" []]) Null `shouldBe` Right (Bool True)
  it "can be refused in lenient mode too" $
    applyWithConfig lenientConfig {unknownOperators = RejectAtCompile} noOperators (call "nope" []) Null
      `shouldSatisfy` \r -> case r of Left (ParseFailure _ (UnknownOperatorName _ _)) -> True; _ -> False
  it "are listed by unresolvedOperators when they are accepted" $
    case compile lazyConfig noOperators (call "and" [call "a" [], call "b" []]) of
      Right ready -> unresolvedOperators ready `shouldBe` [(at [Left "and", Right 0], OperatorName "a"), (at [Left "and", Right 1], OperatorName "b")]
      Left e -> expectationFailure (Text.unpack (renderError e))
  it "are not reported for a registered operator" $
    case compile defaultConfig (registry [("known", anyArity (\_ _ -> Right Null))]) (call "known" []) of
      Right ready -> unresolvedOperators ready `shouldBe` []
      Left e -> expectationFailure (Text.unpack (renderError e))
  it "the declared arity of a custom operator is checked in strict mode" $
    case compile defaultConfig (registry [("two", pureOperator (Exactly 2) (\_ _ -> Right Null))]) (call "two" [Number 1]) of
      Left (ParseFailure _ (ArityMismatch (OperatorName "two") (Exactly 2) 1)) -> pure ()
      other -> expectationFailure ("expected ArityMismatch, got " ++ either show (const "a rule") other)
  it "the declared arity is not checked in lenient mode" $
    applyWithConfig lenientConfig (registry [("two", pureOperator (Exactly 2) (\_ _ -> Right (Number 7)))]) (call "two" [Number 1]) Null
      `shouldBe` Right (Number 7)
  it "apply and compile-then-run agree, for any rule, including those that are refused" $
    property $ forAll (genRule 5) $ \rule -> forAll (genData 3) $ \dat ->
      case compile defaultConfig noOperators rule of
        Left e -> apply rule dat === Left e
        Right ready -> run ready dat === apply rule dat

-- ---------------------------------------------------------------------
-- F-03: numbers a double cannot hold
-- ---------------------------------------------------------------------

f03 :: Spec
f03 = describe "F-03 numbers a double cannot hold" $ do
  let ident = json "{\"var\":\"id\"}"
      dat = json "{\"id\":9007199254740993}"
  it "are rounded by default, as in the reference" $
    apply ident dat `shouldBe` Right (Number 9007199254740992)
  it "keep their digits when passed through, under PreserveSource" $
    applyWithConfig preserve noOperators ident dat `shouldBe` Right (Number 9007199254740993)
  it "keep their digits through map, if and a literal rule" $ do
    applyWithConfig preserve noOperators (json "{\"map\":[{\"var\":\"xs\"},{\"var\":\"\"}]}") (json "{\"xs\":[9007199254740993,1]}")
      `shouldBe` Right (json "[9007199254740993,1]")
    applyWithConfig preserve noOperators (json "{\"if\":[true,9007199254740993,0]}") Null `shouldBe` Right (Number 9007199254740993)
  it "are used as doubles in arithmetic and comparison, whatever the policy" $ do
    applyWithConfig preserve noOperators (json "{\"+\":[{\"var\":\"id\"},0]}") dat `shouldBe` Right (Number 9007199254740992)
    applyWithConfig preserve noOperators (json "{\"==\":[{\"var\":\"id\"},9007199254740992]}") dat `shouldBe` Right (Bool True)
  it "are refused under RejectLossy: in the data, in the rule, and from an operator" $ do
    applyWithConfig rejectLossy noOperators ident dat
      `shouldBe` Left (EvalFailure (fromSegments []) (LossyNumber 9007199254740993))
    applyWithConfig rejectLossy noOperators (Number 9007199254740993) Null
      `shouldBe` Left (ParseFailure (fromSegments []) (LossyNumberLiteral 9007199254740993))
    applyWithConfig rejectLossy (registry [("big", anyArity (\_ _ -> Right (Number 9007199254740993)))]) (call "big" []) Null
      `shouldBe` Left (EvalFailure (fromSegments []) (LossyNumber 9007199254740993))
  it "are not confused with numbers that print back as written" $
    forM_ ["0", "1", "-1", "0.1", "0.5", "1e22", "9007199254740992", "1.7976931348623157e308", "5e-324", "123.456"] $ \text ->
      applyWithConfig rejectLossy noOperators ident (object ["id" .= (json text)]) `shouldBe` Right (json text)
  it "checkData screens the data once, for depth and (under RejectLossy) for lossy numbers" $ do
    checkData defaultConfig dat `shouldBe` Right ()
    checkData rejectLossy dat `shouldBe` Left (EvalFailure (fromSegments []) (LossyNumber 9007199254740993))
    checkData defaultConfig {maxDataDepth = 3} (json "[[[[1]]]]")
      `shouldBe` Left (EvalFailure (fromSegments []) (LimitExceeded (DataDepthLimit 3)))
    checkData defaultConfig {maxDataDepth = 4} (json "[[[[1]]]]") `shouldBe` Right ()
  it "PreserveSource changes nothing for numbers a double holds" $
    property $ forAll (genRule 4) $ \rule -> forAll (genData 3) $ \d ->
      isNothing (firstLossyNumber rule) && isNothing (firstLossyNumber d) ==>
        applyWithConfig preserve noOperators rule d === apply rule d

-- ---------------------------------------------------------------------
-- F-04: a string with a lone surrogate
-- ---------------------------------------------------------------------

f04 :: Spec
f04 = describe "F-04 severed surrogate halves keep their identity" $ do
  let emoji = String "\x1F600"
      hi = call "substr" [emoji, Number 0, Number 1]
      lo = call "substr" [emoji, Number 1, Number 1]
      run1 rule = apply rule Null
  it "the two halves are not equal" $ do
    run1 (call "==" [hi, lo]) `shouldBe` Right (Bool False)
    run1 (call "===" [hi, lo]) `shouldBe` Right (Bool False)
    run1 (call "!=" [hi, lo]) `shouldBe` Right (Bool True)
  it "a half is equal to itself" $
    run1 (call "===" [hi, call "substr" [call "cat" [hi, String "x"], Number 0, Number 1]]) `shouldBe` Right (Bool True)
  it "the halves re-join into the character" $ do
    run1 (call "cat" [hi, lo]) `shouldBe` Right emoji
    run1 (call "===" [call "cat" [hi, lo], emoji]) `shouldBe` Right (Bool True)
  it "the halves order by code unit" $ do
    run1 (call "<" [hi, lo]) `shouldBe` Right (Bool True)
    run1 (call "<" [lo, hi]) `shouldBe` Right (Bool False)
  it "a half is found in the character it came from, and not in another" $ do
    run1 (call "in" [hi, emoji]) `shouldBe` Right (Bool True)
    run1 (call "in" [lo, emoji]) `shouldBe` Right (Bool True)
    run1 (call "in" [hi, String "x"]) `shouldBe` Right (Bool False)
    run1 (call "in" [call "cat" [lo, hi], String "\x1F600\x1F600"]) `shouldBe` Right (Bool True)
  it "a half has one code unit, and slicing it again works" $ do
    run1 (call "substr" [hi, Number 0, Number 5]) `shouldBe` run1 hi
    run1 (call "cat" [call "substr" [call "cat" [hi, String "ab"], Number 1, Number 2]]) `shouldBe` Right (String "ab")
  it "turns into U+FFFD only where JSON cannot carry it, in the result" $
    run1 hi `shouldBe` Right (String "\xFFFD")
  it "a half is not a number and is truthy" $ do
    run1 (call "!!" [hi]) `shouldBe` Right (Bool True)
    run1 (call "+" [hi, Number 1]) `shouldBe` Right Null
  it "an array holding a half converts to a string that holds it" $
    do
      run1 (call "===" [call "cat" [Array (Vector.fromList [hi]), lo], emoji]) `shouldBe` Right (Bool True)
      -- a null element joins as nothing, but the comma between elements stays
      run1 (call "===" [call "cat" [Array (Vector.fromList [hi, Null]), lo], emoji]) `shouldBe` Right (Bool False)
  it "splitting a string anywhere and joining the parts gives it back" $
    property $ forAll genEmojiText $ \s -> forAll (choose (0, utf16Len s)) $ \k ->
      apply
        (call "cat" [call "substr" [String s, Number 0, Number (fromIntegral k)], call "substr" [String s, Number (fromIntegral k)]])
        Null
        === Right (String s)

utf16Len :: Text -> Int
utf16Len s = sum [if fromEnum c > 0xFFFF then 2 else 1 | c <- Text.unpack s]

genEmojiText :: Gen Text
genEmojiText = Text.concat <$> listOf (elements ["a", "b", "\xE9", "\x1F600", "\x1F680", "\x4E2D"])

-- ---------------------------------------------------------------------
-- F-05: limits
-- ---------------------------------------------------------------------

f05 :: Spec
f05 = describe "F-05 one limit model" $ do
  let cfg = defaultConfig {maxIterations = 3}
      body = Bool True
      items :: Int -> Value
      items n = Array (Vector.fromList (map (Number . fromIntegral) [1 .. n]))
      overData rule n = applyWithConfig cfg noOperators (call rule [object ["var" .= ("" :: Text)], body]) (items n)
  forM_ ["map", "filter", "all", "none", "some"] $ \name -> do
    it (Text.unpack name ++ " refuses an array longer than the iteration limit") $
      overData name 4 `shouldBe` Left (EvalFailure root' (LimitExceeded (IterationLimit 3)))
    it (Text.unpack name ++ " accepts an array at the iteration limit") $
      overData name 3 `shouldSatisfy` isRight
  it "reduce refuses an array longer than the iteration limit, like the others" $ do
    let reduceRule = call "reduce" [object ["var" .= ("" :: Text)], object ["var" .= ("accumulator" :: Text)], Number 0]
    applyWithConfig cfg noOperators reduceRule (items 4) `shouldBe` Left (EvalFailure root' (LimitExceeded (IterationLimit 3)))
    applyWithConfig cfg noOperators reduceRule (items 3) `shouldSatisfy` isRight
  it "the iteration limit and the build limit are separate" $ do
    let wide = defaultConfig {maxIterations = 10, maxArrayLength = 2}
        keep n = call "filter" [object ["var" .= ("" :: Text)], call "<" [object ["var" .= ("" :: Text)], Number n]]
    -- keeps one of five: builds one element, so within the build limit
    applyWithConfig wide noOperators (keep 2) (items 5) `shouldBe` Right (json "[1]")
    -- keeps four of five: builds more than the build limit
    applyWithConfig wide noOperators (keep 5) (items 5) `shouldBe` Left (EvalFailure root' (LimitExceeded (ArrayLimit 2)))
    -- map builds as many as it reads
    applyWithConfig wide noOperators (call "map" [object ["var" .= ("" :: Text)], Number 1]) (items 5)
      `shouldBe` Left (EvalFailure root' (LimitExceeded (ArrayLimit 2)))
  it "a negative limit is refused, by name" $ do
    validateConfig defaultConfig {maxSteps = -1} `shouldBe` Left (NegativeLimit "maxSteps")
    validateConfig defaultConfig {maxIterations = -5, maxNodes = -1} `shouldBe` Left (NegativeLimit "maxIterations")
    validateConfig defaultConfig {maxNodes = -1} `shouldBe` Left (NegativeLimit "maxNodes")
    apply' defaultConfig {maxDepth = -1} (Number 1)
      `shouldBe` Left (ParseFailure root' (InvalidConfig (NegativeLimit "maxDepth")))
  it "unlimited is accepted everywhere" $
    applyWithConfig defaultConfig {maxSteps = unlimited, maxDepth = unlimited, maxNodes = unlimited, maxStringLength = unlimited, maxArrayLength = unlimited, maxIterations = unlimited}
      noOperators (call "+" [Number 1, Number 2]) Null
      `shouldBe` Right (Number 3)
  it "zero is a limit, not a switch: it refuses everything it counts" $
    applyWithConfig defaultConfig {maxSteps = 0} noOperators (Number 1) Null
      `shouldBe` Left (EvalFailure root' (LimitExceeded (StepLimit 0)))
  where
    root' = fromSegments []
    apply' config rule = applyWithConfig config noOperators rule Null

-- ---------------------------------------------------------------------
-- F-06: custom operators
-- ---------------------------------------------------------------------

f06 :: Spec
f06 = describe "F-06 the custom operator contract" $ do
  let coalesce = lazyOperator (AtLeast 1) go
        where
          go _ [] = pure Null
          go datum (operand : rest) = do
            value <- force operand
            if value == Null then go datum rest else pure value
      failing = anyArity (\_ _ -> Left (CustomError "boom" "it broke" (Just (Number 1))))
      ops = registry [("coalesce", coalesce), ("fail", failing)]
      fails = call "fail" []
  it "a lazy operator does not evaluate the operands it does not need" $ do
    applyWithConfig defaultConfig ops (call "coalesce" [Number 1, fails]) Null `shouldBe` Right (Number 1)
    applyWithConfig defaultConfig ops (call "coalesce" [Null, Number 2, fails]) Null `shouldBe` Right (Number 2)
  it "a lazy operator evaluates the operands it asks for, and their failures stop it" $
    kind (applyWithConfig defaultConfig ops (call "coalesce" [Null, fails]) Null) `shouldBe` Left "custom_operator_failed"
  it "forcing an operand spends steps from the same budget" $ do
    let rule = call "coalesce" [Null, Null, Null, Number 5]
    applyWithConfig defaultConfig {maxSteps = 100} ops rule Null `shouldBe` Right (Number 5)
    applyWithConfig defaultConfig {maxSteps = 3} ops rule Null
      `shouldSatisfy` \r -> case r of Left (EvalFailure _ (LimitExceeded (StepLimit 3))) -> True; _ -> False
  it "a failure carries its code, message and details unchanged, at the operator" $
    applyWithConfig defaultConfig ops (call "if" [Bool True, fails, Null]) Null
      `shouldBe` Left (EvalFailure (at [Left "if", Right 1]) (CustomOperatorFailed (OperatorName "fail") (CustomError "boom" "it broke" (Just (Number 1)))))
  it "stepFail stops a lazy operator the same way" $ do
    let stopper = lazyOperator (Exactly 0) (\_ _ -> stepFail (CustomError "stop" "stopped" Nothing))
    applyWithConfig defaultConfig (registry [("stop", stopper)]) (call "stop" []) Null
      `shouldBe` Left (EvalFailure (fromSegments []) (CustomOperatorFailed (OperatorName "stop") (CustomError "stop" "stopped" Nothing)))
  it "a lazy operator is handed the datum in scope, and its operands see the same datum" $ do
    let echo = lazyOperator (Exactly 0) (\datum _ -> pure datum)
        firstOf = lazyOperator (Exactly 1) (\_ operands -> case operands of (o : _) -> force o; [] -> pure Null)
        scoped = registry [("echo", echo), ("first_of", firstOf)]
    applyWithConfig defaultConfig scoped (call "map" [json "[1,2]", call "echo" []]) Null `shouldBe` Right (json "[1,2]")
    applyWithConfig defaultConfig scoped (call "map" [json "[[7],[8]]", call "first_of" [object ["var" .= ("0" :: Text)]]]) Null
      `shouldBe` Right (json "[7,8]")
  it "a string literal is a failure with the code \"failed\"" $
    (Left "x" :: Either CustomError ()) `shouldBe` Left (CustomError "failed" "x" Nothing)

-- ---------------------------------------------------------------------
-- F-07: errors
-- ---------------------------------------------------------------------

f07 :: Spec
f07 = describe "F-07 errors" $ do
  let unknown = EvalFailure (at [Left "if", Right 1]) (UnknownOperator (OperatorName "x"))
  it "an uncaught error reads as a sentence" $
    displayException' unknown `shouldBe` "at /if/1: unknown operator \"x\""
  it "an error has a stable JSON form" $
    Aeson.toJSON unknown
      `shouldBe` json "{\"kind\":\"unknown_operator\",\"stage\":\"eval\",\"pointer\":\"/if/1\",\"message\":\"at /if/1: unknown operator \\\"x\\\"\",\"detail\":{\"operator\":\"x\"}}"
  it "every kind has its own tag" $ do
    let errs =
          [ ParseFailure root' (DepthExceeded 1)
          , ParseFailure root' (ArityMismatch (OperatorName "a") (Exactly 1) 0)
          , ParseFailure root' (NodeLimitExceeded 1)
          , ParseFailure root' (UnknownOperatorName (OperatorName "a") [])
          , ParseFailure root' (InvalidConfig (NegativeLimit "x"))
          , ParseFailure root' (LossyNumberLiteral 1)
          , EvalFailure root' (CustomOperatorFailed (OperatorName "a") (customFailure "x"))
          , EvalFailure root' (InvalidOperand (OperatorName "a") (OperandIssue 0 "x" "y"))
          , EvalFailure root' (LimitExceeded (StepLimit 1))
          , EvalFailure root' (LossyNumber 1)
          , EvalFailure root' (UnknownOperator (OperatorName "a"))
          ]
    -- a kind is unique within a stage (a parse and an eval error may share a name)
    let tagged e = (errorKind e, case e of ParseFailure {} -> True; EvalFailure {} -> False)
    length (uniq (map tagged errs)) `shouldBe` length errs
    forM_ errs $ \e -> Aeson.toJSON e `shouldSatisfy` \v -> case v of Object _ -> True; _ -> False
  it "an invalid operand says which, what was needed, and what was given" $ do
    let r = applyWithConfig lenientConfig noOperators (call "missing_some" [Number 1, Null]) Null
    fmap id (either (Left . renderError) Right r)
      `shouldBe` Left "at the root of the rule: operator \"missing_some\": operand 2 must be a list of keys, but is null"
  it "validate reports every problem, in document order" $ do
    let rule = call "and" [call "nope" [], call "==" [Number 1], call "if" [Bool True, call "mox" []]]
    map errorLocation (validate defaultConfig noOperators rule)
      `shouldBe` [at [Left "and", Right 0], at [Left "and", Right 1], at [Left "and", Right 2, Left "if", Right 1]]
  it "validate is empty exactly when compile succeeds, and starts with compile's error" $
    property $ forAll (genRule 5) $ \rule -> forAll (elements [defaultConfig, lenientConfig, lazyConfig]) $ \cfg ->
      case compile cfg noOperators rule of
        Right _ -> validate cfg noOperators rule === []
        Left e -> take 1 (validate cfg noOperators rule) === [e]
  it "validate sees problems in custom operators too" $ do
    let ops = registry [("two", pureOperator (Exactly 2) (\_ _ -> Right Null))]
    map errorKind (validate defaultConfig ops (call "or" [call "two" [Number 1], call "three" []]))
      `shouldBe` ["arity_mismatch", "unknown_operator"]
  where
    root' = fromSegments []
    displayException' = Control.Exception.displayException
    uniq = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

-- ---------------------------------------------------------------------
-- F-10: ordering of locations
-- ---------------------------------------------------------------------

f10 :: Spec
f10 = describe "F-10 locations order by position in the rule" $ do
  it "a key sorts by its own name before a later segment" $
    (fromSegments [Key "a", Key "z"] < fromSegments [Key "b", Key "a"]) `shouldBe` True
  it "a parent sorts before its children, and siblings by index" $ do
    (fromSegments [Key "a"] < fromSegments [Key "a", Index 0]) `shouldBe` True
    (fromSegments [Key "a", Index 1] < fromSegments [Key "a", Index 2]) `shouldBe` True
    (fromSegments [Key "a", Index 9] < fromSegments [Key "b"]) `shouldBe` True
  it "the order is that of the segment lists, whatever they are" $
    property $ forAll (listOf genSegment) $ \xs -> forAll (listOf genSegment) $ \ys ->
      compare (fromSegments xs) (fromSegments ys) === compare xs ys
  it "a parent is smaller than every extension of it" $
    property $ forAll (listOf genSegment) $ \xs -> forAll genSegment $ \s ->
      fromSegments xs < fromSegments (xs ++ [s])
  where
    genSegment = oneof [Key <$> elements ["a", "b", "if", "var"], Index <$> choose (0, 5)]

-- ---------------------------------------------------------------------
-- F-11: registries
-- ---------------------------------------------------------------------

f11 :: Spec
f11 = describe "F-11 operator registries" $ do
  let one = anyArity (\_ _ -> Right Null)
  it "refuse an empty name and a padded one" $ do
    operatorsFromList' [("", one)] `shouldBe` Just (InvalidOperatorName (OperatorName ""))
    operatorsFromList' [(" x", one)] `shouldBe` Just (InvalidOperatorName (OperatorName " x"))
    operatorsFromList' [("x ", one)] `shouldBe` Just (InvalidOperatorName (OperatorName "x "))
  it "combine disjoint registries, and refuse a name defined by both" $ do
    let a = registry [("f", one)]
        b = registry [("g", one)]
    isRight (combineOperators a b) `shouldBe` True
    either Just (const Nothing) (combineOperators a a) `shouldBe` Just (DuplicateOperatorName (OperatorName "f"))
    isRight (combineOperators a noOperators) `shouldBe` True
  where
    operatorsFromList' entries = either Just (const Nothing) (operatorsFromList [(OperatorName n, o) | (n, o) <- entries])

-- ---------------------------------------------------------------------
-- F-13: lenient mode evaluates every operand before applying the operator
-- ---------------------------------------------------------------------

f13 :: Spec
f13 = describe "F-13 lenient surplus operands, in the reference's order" $ do
  it "a surplus operand that fails is reported before the operator's own failure" $
    applyWithConfig lenientConfig noOperators (call "missing_some" [Number 1, Null, call "nope" []]) Null
      `shouldBe` Left (EvalFailure (at [Left "missing_some", Right 2]) (UnknownOperator (OperatorName "nope")))
  it "the operator's own failure is still reported when the surplus is harmless" $
    applyWithConfig lenientConfig noOperators (call "missing_some" [Number 1, Null, Number 3]) Null
      `shouldSatisfy` \r -> case r of Left (EvalFailure _ (InvalidOperand _ _)) -> True; _ -> False
  it "surplus operands are still evaluated, and discarded" $ do
    applyWithConfig lenientConfig noOperators (call "!" [Number 1, Number 2]) Null `shouldBe` Right (Bool False)
    applyWithConfig lenientConfig noOperators (call "!" [Number 1, call "nope" []]) Null `shouldSatisfy` isLeft
  it "the data a rule reads is still found when a var has surplus operands" $
    case compile lenientConfig noOperators (call "var" [String "a", Null, object ["var" .= ("b" :: Text)]]) of
      Right ready -> usedPaths (dataUse ready) `shouldBe` ["a", "b"]
      Left e -> expectationFailure (Text.unpack (renderError e))
  it "lenient and strict agree wherever strict accepts the rule" $
    property $ forAll (genRule 5) $ \rule -> forAll (genData 3) $ \d ->
      case apply rule d of
        Left _ -> property True
        Right r -> applyWithConfig lenientConfig noOperators rule d === Right r

-- ---------------------------------------------------------------------
-- F-15: JSON text in and out
-- ---------------------------------------------------------------------

f15 :: Spec
f15 = describe "F-15 applyBytes" $ do
  it "applies a rule to data, both as JSON text" $
    applyBytes "{\"+\":[{\"var\":\"a\"},1]}" "{\"a\":2}" `shouldSatisfy` \r -> either (const False) (== "3") r
  it "says which of the two is not JSON" $ do
    applyBytes "{" "{}" `shouldSatisfy` \r -> case r of Left (MalformedRule _) -> True; _ -> False
    applyBytes "{}" "}" `shouldSatisfy` \r -> case r of Left (MalformedData _) -> True; _ -> False
  it "reports a refusal as an Error" $
    applyBytes "{\"nope\":[]}" "null" `shouldSatisfy` \r -> case r of Left (Refused (ParseFailure _ _)) -> True; _ -> False
  it "agrees with apply" $
    property $ forAll (genRule 4) $ \rule -> forAll (genData 3) $ \d ->
      case (apply rule d, applyBytes (Aeson.encode rule) (Aeson.encode d)) of
        (Right v, Right bytes) -> Aeson.decode bytes === Just v
        (Left e, Left (Refused e')) -> e === e'
        (a, b) -> counterexample (show (a, b)) False

-- ---------------------------------------------------------------------
-- Sharing a compiled rule
-- ---------------------------------------------------------------------

concurrency :: Spec
concurrency = describe "a compiled rule shared by several threads" $
  it "gives each the answer it would have got alone" $ do
    let rule = json "{\"map\":[{\"var\":\"xs\"},{\"*\":[{\"var\":\"\"},2]}]}"
        inputs = [object ["xs" .= [n .. n + 20]] | n <- [1 .. 40 :: Int]]
    case compile defaultConfig noOperators rule of
      Left e -> expectationFailure (Text.unpack (renderError e))
      Right ready -> do
        boxes <- forM inputs $ \input -> do
          box <- newEmptyMVar
          _ <- forkIO (putMVar box $! run ready input)
          pure box
        results <- mapM takeMVar boxes
        results `shouldBe` map (run ready) inputs
