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
-- Usage: @cabal run jsonlogic-hs-differential -- [COUNT] [SEED]@
--
-- Rules are generated with argument counts this library accepts, so a
-- disagreement is a real behavioural difference, not the documented
-- strict-arity deviation (VISION.md §11). Equality between two values that
-- are arrays or objects is avoided for the same reason: the reference
-- compares them by reference identity, which parsed JSON does not have.
module Main (main) where

import Control.Monad (unless)
import Data.Aeson (Value (..), decode, encode, object, toJSON, (.=))
import Data.Aeson.Key (Key)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Process (callProcess)
import Test.QuickCheck (Gen, choose, elements, frequency, vectorOf)
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)

import JsonLogic (Error, apply)

-- ---------------------------------------------------------------------
-- Generators
-- ---------------------------------------------------------------------

-- | Where a rule sits: at the top, inside a @map@-style operator (where
-- @{"var": ""}@ is the current element), or inside a @reduce@ (where
-- @current@ and @accumulator@ exist).
data Ctx = Top | Iter | Fold

trickyStrings :: [Text]
trickyStrings =
  [ "", "0", "1", "2", "a", "b", "abc", "jsonlogic", " 12 ", "1e3", "0x10"
  , "-3", ".5", "5.", "Infinity", "-Infinity", "NaN", "10", "9", "true"
  , "null", "1,2", "[object Object]", "a.b"
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
    ]
  where
    decimals :: [Double]
    decimals = [0.5, 1.5, -2.25, 100, 1e-7, 1e21]

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
genLeaf ctx = frequency [(4, genScalar), (4, genVar ctx), (1, genArrayOf genScalar)]

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
        ]

-- | Between @low@ and @high@ sub-rules.
genArgs :: Ctx -> Int -> Int -> Int -> Gen [Value]
genArgs ctx depth low high = do
  n <- choose (low, high)
  vectorOf n (genRule ctx (depth - 1))

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
    , (2, call "*" <$> genArgs ctx depth 1 3)
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

data Mismatch = Mismatch Int Value Value String String

reference :: Value -> Reference
reference (Object fields) =
  case (KeyMap.lookup "ok" fields, KeyMap.lookup "error" fields) of
    (Just value, _) -> ReferenceValue value
    (_, Just (String message)) -> ReferenceError message
    _ -> ReferenceError "malformed oracle line"
reference _ = ReferenceError "malformed oracle line"

agree :: Either Error Value -> Reference -> Bool
agree (Right ours) (ReferenceValue theirs) = ours == theirs
agree (Left _) (ReferenceError _) = True
agree _ _ = False

json :: Value -> String
json = Char8.unpack . encode

describe :: Reference -> String
describe (ReferenceValue value) = json value
describe (ReferenceError message) = "error: " ++ Text.unpack message

compareCase :: Int -> (Value, Value) -> Maybe Value -> Maybe Mismatch
compareCase index (rule, input) line =
  case fmap reference line of
    Nothing -> Just (Mismatch index rule input "" "reference line missing or unreadable")
    Just expected
      | agree ours expected -> Nothing
      | otherwise -> Just (Mismatch index rule input (either show json ours) (describe expected))
  where
    ours = apply rule input

render :: Mismatch -> [String]
render (Mismatch index rule input ours theirs) =
  [ "MISMATCH #" ++ show index
  , "  rule      " ++ json rule
  , "  data      " ++ json input
  , "  ours      " ++ ours
  , "  reference " ++ theirs
  ]

-- ---------------------------------------------------------------------
-- Running
-- ---------------------------------------------------------------------

readInt :: Int -> String -> Int
readInt fallback text = case reads text of
  [(n, [])] -> n
  _ -> fallback

parseArgs :: [String] -> (Int, Int)
parseArgs [c, s] = (readInt 2000 c, readInt 1 s)
parseArgs [c] = (readInt 2000 c, 1)
parseArgs _ = (2000, 1)

main :: IO ()
main = do
  args <- getArgs
  let (count, seed) = parseArgs args
      cases = [unGen genCase (mkQCGen (seed * 1000003 + i)) 30 | i <- [1 .. count]]
      dir = "differential/out"
      casesFile = dir ++ "/cases.jsonl"
      referenceFile = dir ++ "/reference.jsonl"
  createDirectoryIfMissing True dir
  LBS.writeFile
    casesFile
    (Char8.unlines [encode (object ["rule" .= rule, "data" .= input]) | (rule, input) <- cases])
  callProcess "node" ["differential/oracle.js", casesFile, referenceFile]
  answered <- Char8.lines <$> LBS.readFile referenceFile
  unless (length answered == count) $ do
    putStrLn
      ( "differential: the reference gave "
          ++ show (length answered)
          ++ " answers for "
          ++ show count
          ++ " cases"
      )
    exitFailure
  let mismatches = [m | Just m <- zipWith3 compareCase [1 ..] cases (map decode answered)]
  mapM_ putStrLn (concatMap render (take 12 mismatches))
  writeFile (dir ++ "/mismatches.txt") (unlines (concatMap render mismatches))
  putStrLn
    ( show (count - length mismatches)
        ++ " of "
        ++ show count
        ++ " random cases agree with the reference (seed "
        ++ show seed
        ++ ")"
    )
  unless (null mismatches) $ do
    putStrLn ("every mismatch is in " ++ dir ++ "/mismatches.txt")
    exitFailure
