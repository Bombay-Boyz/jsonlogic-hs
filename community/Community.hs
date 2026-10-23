{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Scores jsonlogic-hs on the community test suite (@test/community/@, from
-- json-logic/compat-tables) and holds the score to @expected.json@.
--
-- Each case is scored the way the compatibility tables score an
-- implementation (compat-tables, @js-tests/engines/TestRunner.js@):
--
--   * an expected @null@ accepts any falsy result (@null@, @false@, @0@,
--     @""@, @[]@);
--   * numbers are equal within 1e-10;
--   * arrays and objects are compared element by element and key by key;
--   * an expected error passes only if evaluation fails with an error of
--     that type. Errors are named as the suite names them: an unknown
--     operator is @"Unknown Operator"@, a wrong argument count or an operand
--     the operator cannot use is @"Invalid Arguments"@. This library has no
--     error for @NaN@, which the suite's dialect raises and the classic one
--     does not.
--
-- Every case runs in both arity modes. The run fails if any file's case
-- count or pass counts differ from @test/community/expected.json@, in either
-- direction, so the score changes only in a commit that says so. It also
-- fails if a suite file is missing, has no cases, or is not listed in
-- @expected.json@, so it cannot pass by running nothing.
--
-- > cabal test jsonlogic-hs-community --test-show-details=direct
-- > cabal run jsonlogic-hs-community -- --write     # record a new score
module Main (main) where

import Control.Monad (forM, unless, when)
import Data.Aeson (Value (..), eitherDecodeFileStrict, encode, object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.Foldable (toList)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

import JsonLogic

suiteDirectory :: FilePath
suiteDirectory = "test/community/"

expectedPath :: FilePath
expectedPath = suiteDirectory ++ "expected.json"

-- | One case: rule, data, and what the suite expects.
data Case = Case Value Value Expectation

data Expectation = ExpectResult Value | ExpectError Text

-- | A file's case count, and how many pass in strict and lenient mode.
data Score = Score {scoreCases :: Int, scoreStrict :: Int, scoreLenient :: Int}
  deriving (Eq, Show)

main :: IO ()
main = do
  args <- getArgs
  files <- readJson (suiteDirectory ++ "index.json") >>= stringList "index.json"
  when (null files) $ abort "index.json lists no files"
  scores <- forM files $ \file -> do
    cases <- readJson (suiteDirectory ++ Text.unpack file) >>= suiteCases file
    when (null cases) $ abort (Text.unpack file ++ " has no cases")
    pure (file, score cases)
  report scores
  case args of
    ["--write"] -> do
      writeFile expectedPath (renderExpected scores)
      putStrLn ("wrote " ++ expectedPath)
    [] -> compareWithExpected scores
    _ -> abort "usage: jsonlogic-hs-community [--write]"

-- ---------------------------------------------------------------------
-- Reading the suite
-- ---------------------------------------------------------------------

readJson :: FilePath -> IO Value
readJson path = do
  exists <- doesFileExist path
  unless exists $ abort ("required fixture " ++ path ++ " not found (run from the package root)")
  decoded <- eitherDecodeFileStrict path
  either (\reason -> abort ("cannot read " ++ path ++ ": " ++ reason)) pure decoded

stringList :: String -> Value -> IO [Text]
stringList what (Array items) = forM (toList items) $ \case
  String s -> pure s
  _ -> abort (what ++ ": expected a list of file names")
stringList what _ = abort (what ++ ": expected a list of file names")

-- | The cases of a suite file. Strings are section comments; anything else
-- that is not a case object is an error.
suiteCases :: Text -> Value -> IO [Case]
suiteCases file (Array items) = concat <$> mapM entry (toList items)
  where
    entry (String _) = pure []
    entry (Object fields) = case (KeyMap.lookup "rule" fields, KeyMap.lookup "result" fields, KeyMap.lookup "error" fields) of
      (Just rule, Just result, Nothing) -> pure [Case rule (dataOf fields) (ExpectResult result)]
      (Just rule, Nothing, Just (Object e)) | Just (String kind) <- KeyMap.lookup "type" e -> pure [Case rule (dataOf fields) (ExpectError kind)]
      _ -> abort (Text.unpack file ++ ": a case needs a rule and exactly one of result and error")
    entry _ = abort (Text.unpack file ++ ": an entry is neither a case nor a comment")
    dataOf fields = fromMaybe Null (KeyMap.lookup "data" fields)
suiteCases file _ = abort (Text.unpack file ++ " is not a JSON array")

-- ---------------------------------------------------------------------
-- Scoring
-- ---------------------------------------------------------------------

score :: [Case] -> Score
score cases =
  Score
    (length cases)
    (length (filter (passes defaultConfig) cases))
    (length (filter (passes defaultConfig{arityMode = Lenient}) cases))

passes :: Config -> Case -> Bool
passes config (Case rule input expectation) =
  case (applyWithConfig config noOperators rule input, expectation) of
    (Right got, ExpectResult wanted) -> sameValue got wanted
    (Left failure, ExpectError kind) -> Text.toLower kind `Text.isInfixOf` Text.toLower (errorType failure)
    _ -> False

-- | The suite's name for a kind of failure.
errorType :: Error -> Text
errorType failure = case failure of
  ParseFailure _ (ArityMismatch{}) -> "Invalid Arguments"
  ParseFailure _ (DepthExceeded _) -> "Limit Exceeded"
  EvalFailure _ (UnknownOperator _) -> "Unknown Operator"
  EvalFailure _ (InvalidOperand _) -> "Invalid Arguments"
  EvalFailure _ (CustomOperatorFailed _ reason) -> reason
  EvalFailure _ (LimitExceeded _) -> "Limit Exceeded"

-- | compat-tables' @compareValues@.
sameValue :: Value -> Value -> Bool
sameValue got Null = falsy got
sameValue (Number a) (Number b) = abs (realToFrac a - realToFrac b :: Double) < 1e-10
sameValue (Array a) (Array b) = Vector.length a == Vector.length b && and (Vector.zipWith sameValue a b)
sameValue (Object a) (Object b) =
  KeyMap.size a == KeyMap.size b
    && and [maybe False (sameValue v) (KeyMap.lookup k b) | (k, v) <- KeyMap.toList a]
sameValue got wanted = got == wanted

falsy :: Value -> Bool
falsy Null = True
falsy (Bool False) = True
falsy (Number 0) = True
falsy (String "") = True
falsy (Array items) = Vector.null items
falsy _ = False

-- ---------------------------------------------------------------------
-- Reporting and pinning
-- ---------------------------------------------------------------------

report :: [(Text, Score)] -> IO ()
report scores = do
  printf "%-36s %6s %8s %8s\n" ("suite" :: String) ("cases" :: String) ("strict" :: String) ("lenient" :: String)
  mapM_ (\(file, Score n s l) -> printf "%-36s %6d %8d %8d\n" (Text.unpack file) n s l) scores
  let Score n s l = foldr (\(_, Score a b c) (Score x y z) -> Score (a + x) (b + y) (c + z)) (Score 0 0 0) scores
  printf "%-36s %6d %8d %8d\n" ("total" :: String) n s l

-- | @expected.json@: one line per file, in the suite's order, so a change
-- to the score is a readable one-line diff.
renderExpected :: [(Text, Score)] -> String
renderExpected scores = "{\n" ++ intercalate ",\n" (map line scores) ++ "\n}\n"
  where
    line (file, Score n s l) =
      "  " ++ Char8.unpack (encode (String file)) ++ ": " ++ Char8.unpack (encode (object ["cases" .= n, "strict" .= s, "lenient" .= l]))

compareWithExpected :: [(Text, Score)] -> IO ()
compareWithExpected scores = do
  expected <- readJson expectedPath
  pinned <- case expected of
    Object fields -> forM (KeyMap.toList fields) $ \(k, v) -> do
      pin <- parseScore (Key.toText k) v
      pure (Key.toText k, pin)
    _ -> abort (expectedPath ++ " is not an object")
  let differences =
        [ Text.unpack file ++ ": expected " ++ show want ++ ", got " ++ maybe "nothing (not in the suite)" show got
        | (file, want) <- pinned
        , let got = lookup file scores
        , got /= Just want
        ]
          ++ [ Text.unpack file ++ ": not in " ++ expectedPath
             | (file, _) <- scores
             , file `notElem` map fst pinned
             ]
  unless (null differences) $ do
    mapM_ (hPutStrLn stderr . ("community: " ++)) differences
    abort "the score differs from expected.json; if the change is intended, run with --write and commit the result"
  putStrLn "community: the score matches expected.json"
  where
    parseScore _ (Object o)
      | Just (Number n) <- KeyMap.lookup "cases" o
      , Just (Number s) <- KeyMap.lookup "strict" o
      , Just (Number l) <- KeyMap.lookup "lenient" o =
          pure (Score (round n) (round s) (round l))
    parseScore file _ = abort (expectedPath ++ ": bad entry for " ++ Text.unpack file)

abort :: String -> IO a
abort message = hPutStrLn stderr ("community: " ++ message) >> exitFailure
