-- |
-- Conformance runner for the classic JsonLogic test suite
-- (@test/tests.json@, from jsonlogic.com/tests.json).
--
-- The suite is a JSON array. Each case is a three-element array
-- @[rule, data, expected]@; bare strings are section comments. Every
-- case is run through 'apply' and its result compared with @expected@.
--
-- This runner cannot pass by running nothing. It exits non-zero if:
--
--   * the fixture is missing, unreadable, or not a JSON array;
--   * any entry is neither a three-element case nor a string comment;
--   * the number of cases is not exactly 'pinnedCases'; or
--   * any case fails.
--
-- The classic suite passes in full today. Should a change make a case
-- fail, it is either a bug to fix or a deliberate deviation to record
-- in docs/COMPATIBILITY.md: a human call, not a threshold. If the suite is
-- ever updated, change 'pinnedCases' in the same commit.
module Main (main) where

import Control.Monad (unless)
import Data.Aeson (Value (..), eitherDecodeFileStrict, encode)
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.Foldable (for_, toList)
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import JsonLogic (apply)

suitePath :: FilePath
suitePath = "test/tests.json"

-- | The exact number of cases in the vendored suite.
pinnedCases :: Int
pinnedCases = 278

main :: IO ()
main = do
  exists <- doesFileExist suitePath
  unless exists $
    abort
      ( "required fixture "
          ++ suitePath
          ++ " not found (run from the package root).\n"
          ++ "  The classic suite is vendored in the repository; restore it with:\n"
          ++ "  git checkout -- "
          ++ suitePath
      )
  decoded <- eitherDecodeFileStrict suitePath
  case decoded of
    Left reason -> abort ("cannot read " ++ suitePath ++ ": " ++ reason)
    Right (Array entries) -> report (toList entries)
    Right _ -> abort (suitePath ++ " is not a JSON array")

abort :: String -> IO a
abort message = hPutStrLn stderr ("conformance: " ++ message) >> exitFailure

-- | A case, a comment, or neither.
data Entry = Case Value Value Value | Comment | Malformed Value

classify :: Value -> Entry
classify (Array parts) = case toList parts of
  [rule, input, expected] -> Case rule input expected
  _ -> Malformed (Array parts)
classify (String _) = Comment
classify other = Malformed other

report :: [Value] -> IO ()
report entries = do
  let classified = map classify entries
      cases = [(rule, input, expected) | Case rule input expected <- classified]
      malformed = [v | Malformed v <- classified]
      failures =
        [ ((rule, input, expected), outcome)
        | (rule, input, expected) <- cases
        , let outcome = apply rule input
        , outcome /= Right expected
        ]
  for_ malformed $ \v ->
    hPutStrLn stderr ("conformance: entry is neither a case nor a comment: " ++ render v)
  for_ failures $ \((rule, input, expected), outcome) -> do
    putStrLn ("FAIL  " ++ render rule ++ "  on  " ++ render input)
    putStrLn ("      expected " ++ render expected)
    putStrLn ("      got      " ++ either show render outcome)
  putStrLn
    ( show (length cases - length failures)
        ++ " of "
        ++ show (length cases)
        ++ " cases pass; "
        ++ show (length entries - length cases - length malformed)
        ++ " comments skipped."
    )
  unless (length cases == pinnedCases) $
    abort
      ( "expected exactly "
          ++ show pinnedCases
          ++ " cases but found "
          ++ show (length cases)
          ++ ": the fixture is truncated or changed"
      )
  unless (null malformed) exitFailure
  unless (null failures) exitFailure

render :: Value -> String
render = Char8.unpack . encode
