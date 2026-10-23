{-# LANGUAGE OverloadedStrings #-}

-- |
-- Differential test: run randomly generated rules through this library and
-- through the real JavaScript implementation (json-logic-js, via Node), and
-- report every disagreement.
--
-- The classic suite checks the reference's own examples. This checks
-- everything else: it generates thousands of rules over every builtin
-- operator, fed deliberately awkward values (@"0x10"@, @" 12 "@, @"1e3"@,
-- @null@, empty strings), and compares the two answers.
--
-- Usage:
--
-- > cabal run jsonlogic-hs-differential -- COUNT SEED
-- > cabal run jsonlogic-hs-differential -- --replay FILE
--
-- The seed is always printed first, so any run can be repeated. A failing
-- case is shrunk against the reference to a smaller rule that still
-- disagrees, and written to a file that @--replay@ re-checks.
--
-- Cases are compared in lenient arity mode, which is the mode that models
-- the reference (docs/COMPATIBILITY.md); in strict mode the library must
-- give the same answer unless it rejects the arity. Equality between two
-- values that are arrays or objects is avoided in generated rules: the
-- reference compares them by reference identity, which parsed JSON does
-- not have.
--
-- An error is compared with an error only when both are of the same
-- class (unknown operator, invalid operand, empty @*@). A reference error
-- of any other kind fails the run, so a new kind of failure is noticed
-- rather than silently accepted.
module Main (main) where

import Control.Monad (unless, when, (>=>))
import Data.Aeson (Value (..), decode, encode, object, toJSON, (.=))
import Data.Aeson.Key (Key)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hSetEncoding, stderr, stdout, utf8)
import System.Process (callProcess)
import Test.QuickCheck (Gen, choose, elements, frequency, vectorOf)
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)

import JsonLogic

-- ---------------------------------------------------------------------
-- Generators
-- ---------------------------------------------------------------------

-- | Where a rule sits: at the top, inside a @map@-style operator (where
-- @{"var": ""}@ is the current element), or inside a @reduce@ (where
-- @current@ and @accumulator@ exist).
data Ctx = Top | Iter | Fold

trickyStrings :: [Text]
trickyStrings =
  [ ""
  , "0"
  , "1"
  , "2"
  , "a"
  , "b"
  , "abc"
  , "jsonlogic"
  , " 12 "
  , "1e3"
  , "0x10"
  , "-3"
  , ".5"
  , "5."
  , "Infinity"
  , "-Infinity"
  , "NaN"
  , "10"
  , "9"
  , "true"
  , "null"
  , "1,2"
  , "[object Object]"
  , "a.b"
  , "\x00a0\&7"
  , "\xfeff\&3"
  , "\x2028 4 \x2029"
  , "\x0085\&5"
  , "\x200b\&6"
  , "0x1F"
  , "0b101"
  , "1e1000"
  , "\x1F600"
  , "a\x1F600\&b"
  , "\xE000"
  , "e\x0301"
  , "\x65E5\x672C"
  , "9007199254740993"
  ]

-- | Characters either side of the UTF-16 surrogate range, where ordering
-- by code unit and by code point part ways.
unicodeEdges :: [Text]
unicodeEdges = ["\x1F600", "\xE000", "\xFFFF", "\x10000", "\xD7FF", "a", "", "\x1F600\&a", "\x10FFFF"]

-- | Each ECMAScript whitespace character, and some that look like it,
-- followed by a digit.
whitespaceEdges :: [Text]
whitespaceEdges =
  [ Text.pack [c, '5']
  | c <-
      [ '\t'
      , '\n'
      , '\v'
      , '\f'
      , '\r'
      , ' '
      , '\xA0'
      , '\x1680'
      , '\x2000'
      , '\x2005'
      , '\x200A'
      , '\x2028'
      , '\x2029'
      , '\x202F'
      , '\x205F'
      , '\x3000'
      , '\xFEFF'
      , '\x85'
      , '\x180E'
      , '\x200B'
      ]
  ]

scalarPaths :: [Text]
scalarPaths =
  ["a", "b", "c", "o.x", "o.y", "l.0", "l.1", "l.2", "l.length", "z", "a.b", "o.z"]

-- | A scalar chosen to provoke JavaScript's coercion rules.
genScalar :: Gen Value
genScalar =
  frequency
    [ (1, pure Null)
    , (2, Bool <$> elements [True, False])
    , (4, toJSON <$> (choose (-6, 12) :: Gen Int))
    , (2, toJSON <$> elements decimals)
    , (5, String <$> elements trickyStrings)
    , (1, elements extremes)
    ]
  where
    decimals :: [Double]
    decimals = [0.5, 1.5, -2.25, 100, 1e-7, 1e21]
    extremes =
      [ Number 5e-324
      , Number 1.7976931348623157e308
      , Number 2.2250738585072014e-308
      , Number 9007199254740993
      , Number 1e309
      , Number 123456789012345678901234567890
      ]

genArrayOf :: Gen Value -> Gen Value
genArrayOf element = do
  n <- choose (0, 3)
  toJSON <$> vectorOf n element

call :: Key -> [Value] -> Value
call op args = object [op .= args]

genPath :: Ctx -> Gen Value
genPath ctx =
  frequency
    ( [ (6, String <$> elements scalarPaths)
      , (1, toJSON <$> (choose (0, 2) :: Gen Int))
      ]
        ++ extra
    )
  where
    extra = case ctx of
      Top -> []
      Iter -> [(8, pure (String ""))]
      Fold -> [(6, pure (String "current")), (6, pure (String "accumulator"))]

genVar :: Ctx -> Gen Value
genVar ctx = do
  path <- genPath ctx
  withFallback <- elements [True, False, False]
  if withFallback
    then do
      fallback <- genScalar
      pure (call "var" [path, fallback])
    else pure (call "var" [path])

genLeaf :: Ctx -> Gen Value
genLeaf ctx = frequency [(4, genScalar), (4, genVar ctx), (1, genArrayOf genScalar), (1, elements computed)]

-- | Values JSON cannot write (-0, NaN, the infinities, undefined), produced
-- by a rule instead. They belong in rules only: put in the data they would
-- be objects, which the reference compares by identity.
computed :: [Value]
computed =
  [ object ["*" .= [toJSON (-1 :: Int), toJSON (0 :: Int)]]
  , object ["+" .= [String "a"]]
  , object ["/" .= [toJSON (1 :: Int), toJSON (0 :: Int)]]
  , object ["/" .= [toJSON (-1 :: Int), toJSON (0 :: Int)]]
  , object ["and" .= ([] :: [Int])]
  ]

genRule :: Ctx -> Int -> Gen Value
genRule ctx depth
  | depth <= 0 = genLeaf ctx
  | otherwise =
      frequency
        [ (3, genLeaf ctx)
        , (2, genLogic ctx depth)
        , (3, genCompare ctx depth)
        , (3, genArith ctx depth)
        , (2, genText ctx depth)
        , (2, genScoped ctx depth)
        , (1, genReduce ctx depth)
        , (1, genMerge ctx depth)
        , (1, genMissing)
        , (2, genIf ctx depth)
        , (1, genArrayRule ctx depth)
        , (2, genStringFocus)
        ]

-- | Between @low@ and @high@ sub-rules.
genArgs :: Ctx -> Int -> Int -> Int -> Gen [Value]
genArgs ctx depth low high = do
  n <- choose (low, high)
  vectorOf n (genRule ctx (depth - 1))

-- | Operators applied straight to awkward strings, which is where
-- JavaScript's coercion and ordering rules bite hardest.
genStringFocus :: Gen Value
genStringFocus = do
  pool <- elements [trickyStrings, unicodeEdges, whitespaceEdges]
  a <- String <$> elements pool
  b <- String <$> elements pool
  frequency
    [ (3, (\op -> call op [a, b]) <$> elements ["<", "<=", ">", ">=", "==", "!=", "===", "in", "cat", "max", "min"])
    , (2, (\op -> call op [a]) <$> elements ["+", "-", "!", "!!", "max", "min", "*"])
    , (2, (\op -> call op [a, toJSON (1 :: Int)]) <$> elements ["+", "*", "/", "%", "-", "<", "=="])
    ]

genLogic :: Ctx -> Int -> Gen Value
genLogic ctx depth =
  frequency
    [ (3, call <$> elements ["and", "or"] <*> genArgs ctx depth 0 3)
    , (2, genUnary ctx depth)
    ]

-- | @!@ or @!!@, written either with an argument list or, when the
-- argument is not itself an array, in the bare-value shorthand.
genUnary :: Ctx -> Int -> Gen Value
genUnary ctx depth = do
  op <- elements ["!", "!!"]
  x <- genRule ctx (depth - 1)
  shorthand <- elements [True, False]
  pure $ case x of
    Array _ -> call op [x]
    _
      | shorthand -> object [op .= x]
      | otherwise -> call op [x]

genCompare :: Ctx -> Int -> Gen Value
genCompare ctx depth =
  frequency
    [ (4, call <$> elements ["==", "===", "!=", "!=="] <*> genArgs ctx depth 2 2)
    , (3, call <$> elements ["<", "<="] <*> genArgs ctx depth 2 3)
    , (3, call <$> elements [">", ">="] <*> genArgs ctx depth 2 2)
    ]

genArith :: Ctx -> Int -> Gen Value
genArith ctx depth =
  frequency
    [ (3, call "+" <$> genArgs ctx depth 0 3)
    , -- zero operands too: the reference throws on an empty @*@ only when
      -- it is evaluated, so one in an untaken branch must not fail the rule
      (2, call "*" <$> genArgs ctx depth 0 3)
    , (2, call "-" <$> genArgs ctx depth 1 2)
    , (2, call <$> elements ["/", "%"] <*> genArgs ctx depth 2 2)
    , (2, call <$> elements ["max", "min"] <*> genArgs ctx depth 0 3)
    ]

genText :: Ctx -> Int -> Gen Value
genText ctx depth =
  frequency
    [ (3, call "cat" <$> genArgs ctx depth 0 3)
    , (3, genSubstr ctx depth)
    , (2, genIn ctx depth)
    ]

genIndex :: Gen Value
genIndex =
  frequency
    [ (4, toJSON <$> (choose (-7, 7) :: Gen Int))
    , (1, String <$> elements ["1", "-2", "x"])
    , (1, pure Null)
    ]

genSubstr :: Ctx -> Int -> Gen Value
genSubstr ctx depth = do
  source <- frequency [(2, String <$> elements trickyStrings), (1, genRule ctx (depth - 1))]
  start <- genIndex
  withLength <- elements [True, False]
  if withLength
    then do
      len <- genIndex
      pure (call "substr" [source, start, len])
    else pure (call "substr" [source, start])

genIn :: Ctx -> Int -> Gen Value
genIn ctx depth = do
  needle <- genRule ctx (depth - 1)
  haystack <-
    frequency
      [ (3, String <$> elements trickyStrings)
      , (3, genArrayOf genScalar)
      , (2, pure (call "var" [String "l"]))
      , (1, genRule ctx (depth - 1))
      ]
  pure (call "in" [needle, haystack])

genCollectionSource :: Ctx -> Gen Value
genCollectionSource ctx =
  frequency
    [ (4, pure (call "var" [String "l"]))
    , (3, genArrayOf genScalar)
    , (1, pure (call "var" [String "o"]))
    , (1, genVar ctx)
    ]

genScoped :: Ctx -> Int -> Gen Value
genScoped ctx depth = do
  op <- elements ["map", "filter", "all", "none", "some"]
  collection <- genCollectionSource ctx
  logic <- genRule Iter (depth - 1)
  pure (call op [collection, logic])

genReduce :: Ctx -> Int -> Gen Value
genReduce ctx depth = do
  collection <- genCollectionSource ctx
  logic <- genRule Fold (depth - 1)
  initial <- genScalar
  pure (call "reduce" [collection, logic, initial])

genMerge :: Ctx -> Int -> Gen Value
genMerge ctx depth = do
  n <- choose (0, 3)
  parts <- vectorOf n (frequency [(3, genArrayOf genScalar), (2, genRule ctx (depth - 1))])
  pure (call "merge" parts)

genMissing :: Gen Value
genMissing = do
  n <- choose (0, 3)
  keys <- vectorOf n (String <$> elements scalarPaths)
  need <- choose (0, 3) :: Gen Int
  elements [call "missing" keys, call "missing_some" [toJSON need, toJSON keys]]

genIf :: Ctx -> Int -> Gen Value
genIf ctx depth = do
  op <- elements ["if", "if", "?:"]
  call op <$> genArgs ctx depth 0 5

-- | An array literal whose elements are themselves rules.
genArrayRule :: Ctx -> Int -> Gen Value
genArrayRule ctx depth = do
  n <- choose (0, 3)
  toJSON <$> vectorOf n (genRule ctx (depth - 1))

genObjectData :: Gen Value
genObjectData = do
  a <- genScalar
  b <- genScalar
  c <- genScalar
  n <- choose (0, 4)
  items <- vectorOf n genScalar
  x <- genScalar
  y <- genScalar
  present <- vectorOf 5 (elements [True, True, True, True, False])
  let values = [a, b, c, toJSON items, object ["x" .= x, "y" .= y]]
      fields = [(key, value) | (key, value, True) <- zip3 ["a", "b", "c", "l", "o"] values present]
  pure (object [key .= value | (key, value) <- fields])

genData :: Gen Value
genData =
  frequency
    [ (14, genObjectData)
    , (1, pure Null)
    , (1, genScalar)
    , (1, genArrayOf genScalar)
    ]

genCase :: Gen (Value, Value)
genCase = do
  depth <- choose (1, 4)
  rule <- genRule Top depth
  input <- genData
  pure (rule, input)

-- ---------------------------------------------------------------------
-- Comparison
-- ---------------------------------------------------------------------

-- | What the reference answered.
data Reference = ReferenceValue Value | ReferenceError Text

reference :: Value -> Reference
reference (Object fields) =
  case (KeyMap.lookup "ok" fields, KeyMap.lookup "error" fields) of
    (Just value, _) -> ReferenceValue value
    (_, Just (String message)) -> ReferenceError message
    _ -> ReferenceError "malformed oracle line"
reference _ = ReferenceError "malformed oracle line"

-- | The library's answer in both arity modes.
data Ours = Ours
  { lenientAnswer :: Either Error Value
  , strictAnswer :: Either Error Value
  }

answers :: (Value, Value) -> Ours
answers (rule, input) =
  Ours
    (applyWithConfig defaultConfig{arityMode = Lenient} noOperators rule input)
    (applyWithConfig defaultConfig noOperators rule input)

-- | Why the answers disagree, if they do.
judge :: Reference -> Ours -> Maybe String
judge expected ours = case (judgeOne expected (lenientAnswer ours), strictAnswer ours) of
  (Just why, _) -> Just ("lenient: " ++ why)
  (Nothing, Left (ParseFailure _ ArityMismatch{})) -> Nothing
  (Nothing, strictResult) -> ("strict: " ++) <$> judgeOne expected strictResult

judgeOne :: Reference -> Either Error Value -> Maybe String
judgeOne (ReferenceValue theirs) (Right ours)
  | ours == theirs = Nothing
  | otherwise = Just "different value"
judgeOne (ReferenceValue _) (Left e) = Just ("the library failed: " ++ Text.unpack (renderError e))
judgeOne (ReferenceError _) (Right _) = Just "the reference throws but the library answered"
judgeOne (ReferenceError message) (Left e) = case errorClass e of
  Just wanted
    | any (`isInfixOf` Text.unpack message) wanted -> Nothing
    | otherwise -> Just ("error of a different class: " ++ Text.unpack (renderError e))
  Nothing -> Just ("the library failed in a way the reference does not: " ++ Text.unpack (renderError e))

-- | The messages the reference gives for each class of library error.
errorClass :: Error -> Maybe [String]
errorClass (EvalFailure _ (UnknownOperator _)) = Just ["Unrecognized operation", "apply is not a function"]
errorClass (EvalFailure _ (InvalidOperand (OperatorName "*"))) = Just ["Reduce of empty array"]
errorClass (EvalFailure _ (InvalidOperand _)) = Just ["Cannot read properties"]
errorClass _ = Nothing

json :: Value -> String
json = Char8.unpack . encode

describe :: Reference -> String
describe (ReferenceValue value) = json value
describe (ReferenceError message) = "error: " ++ Text.unpack message

describeOurs :: Either Error Value -> String
describeOurs = either (\e -> "error: " ++ Text.unpack (renderError e)) json

-- ---------------------------------------------------------------------
-- Asking the reference, and shrinking
-- ---------------------------------------------------------------------

-- | Ask the reference about a batch of cases with one Node process.
askReference :: FilePath -> [(Value, Value)] -> IO [Reference]
askReference stem cases = do
  let casesFile = stem ++ ".cases.jsonl"
      referenceFile = stem ++ ".reference.jsonl"
  LBS.writeFile
    casesFile
    (Char8.unlines [encode (object ["rule" .= rule, "data" .= input]) | (rule, input) <- cases])
  callProcess "node" ["differential/oracle.js", casesFile, referenceFile]
  answered <- Char8.lines <$> LBS.readFile referenceFile
  unless (length answered == length cases) $ do
    putStrLn
      ( "differential: the reference gave "
          ++ show (length answered)
          ++ " answers for "
          ++ show (length cases)
          ++ " cases"
      )
    exitFailure
  pure (map (maybe (ReferenceError "malformed oracle line") reference . decode) answered)

-- | Simpler variants of a case: a rule replaced by one of its operands, an
-- operand dropped, a literal made simpler, the data emptied.
shrinks :: (Value, Value) -> [(Value, Value)]
shrinks (rule, input) =
  [(r, input) | r <- shrinkValue rule]
    ++ [(rule, d) | d <- shrinkData input]

shrinkData :: Value -> [Value]
shrinkData Null = []
shrinkData (Object fields) =
  Null : [Object (KeyMap.delete k fields) | k <- KeyMap.keys fields]
shrinkData _ = [Null]

shrinkValue :: Value -> [Value]
shrinkValue value = case value of
  Object fields -> case KeyMap.toList fields of
    [(key, args)] ->
      let operands = case args of
            Array items -> foldr (:) [] items
            other -> [other]
       in operands
            ++ [object [key .= toJSON (dropAt i operands)] | i <- [0 .. length operands - 1]]
            ++ [object [key .= toJSON (replaceAt i smaller operands)] | (i, o) <- zip [0 ..] operands, smaller <- shrinkValue o]
    _ -> [Null]
  Array items ->
    let elems = foldr (:) [] items
     in elems ++ [toJSON (dropAt i elems) | i <- [0 .. length elems - 1]]
  String t | not (Text.null t) -> [String "", String (Text.take 1 t)]
  Number n | n /= 0 -> [Number 0, Number 1] ++ [Number (n / 2) | isSmall n]
  Bool True -> [Bool False]
  Null -> []
  _ -> [Null]
  where
    isSmall :: Scientific -> Bool
    isSmall n = abs n < 1e15

dropAt :: Int -> [a] -> [a]
dropAt i xs = take i xs ++ drop (i + 1) xs

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i x xs = take i xs ++ [x] ++ drop (i + 1) xs

-- | Greedily replace a failing case by a simpler one that still fails.
shrinkCase :: FilePath -> (Value, Value) -> IO (Value, Value)
shrinkCase stem = go (60 :: Int)
  where
    go 0 current = pure current
    go fuel current = do
      let candidates = take 400 (shrinks current)
      if null candidates
        then pure current
        else do
          expected <- askReference stem candidates
          let failing = [c | (c, e) <- zip candidates expected, Just _ <- [judge e (answers c)]]
          case failing of
            [] -> pure current
            c : _ -> go (fuel - 1) c

-- ---------------------------------------------------------------------
-- Running
-- ---------------------------------------------------------------------

readInt :: Int -> String -> Int
readInt fallback text = case reads text of
  [(n, [])] -> n
  _ -> fallback

outDir :: FilePath
outDir = "differential/out"

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  args <- getArgs
  createDirectoryIfMissing True outDir
  case args of
    ["--replay", file] -> replay file
    [c, s] -> randomRun (readInt 2000 c) (readInt 1 s)
    [c] -> randomRun (readInt 2000 c) 1
    _ -> randomRun 2000 1

-- | Re-check the cases in a file, one @{"rule":..,"data":..}@ per line.
replay :: FilePath -> IO ()
replay file = do
  contents <- Char8.lines <$> LBS.readFile file
  let cases = mapMaybe (decode >=> toCase) contents
  when (null cases) $ putStrLn ("differential: no cases in " ++ file) >> exitFailure
  expected <- askReference (outDir ++ "/replay") cases
  let bad = [(c, e, judge e (answers c)) | (c, e) <- zip cases expected]
      failures = [(c, e, why) | (c, e, Just why) <- bad]
  mapM_ (reportFailure Nothing) (zip [1 ..] failures)
  putStrLn (show (length cases - length failures) ++ " of " ++ show (length cases) ++ " replayed cases agree")
  unless (null failures) exitFailure
  where
    toCase (Object o) = (,) <$> KeyMap.lookup "rule" o <*> pure (fromMaybe Null (KeyMap.lookup "data" o))
    toCase _ = Nothing

randomRun :: Int -> Int -> IO ()
randomRun count seed = do
  putStrLn ("differential: seed " ++ show seed ++ ", " ++ show count ++ " cases")
  let cases = [unGen genCase (mkQCGen (seed * 1000003 + i)) 30 | i <- [1 .. count]]
  expected <- askReference (outDir ++ "/run") cases
  let bad = [(i, c, e, why) | (i, c, e) <- zip3 [1 :: Int ..] cases expected, Just why <- [judge e (answers c)]]
  putStrLn
    ( show (count - length bad)
        ++ " of "
        ++ show count
        ++ " random cases agree with the reference (seed "
        ++ show seed
        ++ ")"
    )
  unless (null bad) $ do
    -- shrink and report the first few; list every index
    mapM_
      ( \(i, c, _, _) -> do
          small <- shrinkCase (outDir ++ "/shrink") c
          [e] <- askReference (outDir ++ "/shrink1") [small]
          let why = fromMaybe "no longer disagrees after shrinking" (judge e (answers small))
          putStrLn ("  original  " ++ json (fst c) ++ "  on  " ++ json (snd c))
          reportFailure (Just (seed, i)) (i, (small, e, why))
      )
      (take 5 [(i, c, e, w) | (i, c, e, w) <- bad])
    writeFile
      (outDir ++ "/mismatches.txt")
      (unlines ["seed " ++ show seed ++ " index " ++ show i ++ ": " ++ why | (i, _, _, why) <- bad])
    putStrLn (show (length bad) ++ " mismatches; indices are in " ++ outDir ++ "/mismatches.txt")
    exitFailure

reportFailure :: Maybe (Int, Int) -> (Int, ((Value, Value), Reference, String)) -> IO ()
reportFailure origin (n, ((rule, input), expected, why)) = do
  let reproFile = outDir ++ "/repro-" ++ show n ++ ".jsonl"
  LBS.writeFile reproFile (encode (object ["rule" .= rule, "data" .= input]) <> "\n")
  let ours = answers (rule, input)
  mapM_
    putStrLn
    [ "MISMATCH #" ++ show n ++ maybe "" (\(s, i) -> " (seed " ++ show s ++ ", case " ++ show i ++ ", shrunk)") origin
    , "  why       " ++ why
    , "  rule      " ++ json rule
    , "  data      " ++ json input
    , "  lenient   " ++ describeOurs (lenientAnswer ours)
    , "  strict    " ++ describeOurs (strictAnswer ours)
    , "  reference " ++ describe expected
    , "  repeat    cabal run jsonlogic-hs-differential -- --replay " ++ reproFile
    ]
