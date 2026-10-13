-- |
-- Conformance runner for the classic JsonLogic test suite
-- (@test/tests.json@, from jsonlogic.com/tests.json).
--
-- The suite is a JSON array. Each case is a three-element array
-- @[rule, data, expected]@; bare strings are section comments. Every
-- case is run through 'apply' and its result compared with @expected@.
--
-- It exits non-zero if any case fails, so a regression fails the build.
-- The classic suite passes in full today. Should a change make a case
-- fail, it is either a bug to fix or a deliberate deviation to record
-- in VISION.md §11 — a human call, not a threshold.
module Main (main) where

import Control.Monad (unless)
import Data.Aeson (Value (..), eitherDecodeFileStrict, encode)
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.Foldable (for_, toList)
import System.Directory (doesFileExist)
import System.Exit (exitFailure)

import JsonLogic (apply)

suitePath :: FilePath
suitePath = "test/tests.json"

main :: IO ()
main = do
  exists <- doesFileExist suitePath
  if exists then run else putStrLn missing

missing :: String
missing =
  "conformance: "
    ++ suitePath
    ++ " not found, nothing to check.\n"
    ++ "  Fetch the classic suite with:\n"
    ++ "  curl -L https://jsonlogic.com/tests.json -o "
    ++ suitePath

run :: IO ()
run = do
  decoded <- eitherDecodeFileStrict suitePath
  case decoded of
    Left reason -> do
      putStrLn ("conformance: cannot read " ++ suitePath ++ ": " ++ reason)
      exitFailure
    Right (Array entries) -> report (toList entries)
    Right _ -> do
      putStrLn ("conformance: " ++ suitePath ++ " is not a JSON array")
      exitFailure

report :: [Value] -> IO ()
report entries = do
  let cases =
        [ (rule, input, expected)
        | Array parts <- entries
        , [rule, input, expected] <- [toList parts]
        ]
      failures =
        [ ((rule, input, expected), outcome)
        | (rule, input, expected) <- cases
        , let outcome = apply rule input
        , outcome /= Right expected
        ]
  for_ failures $ \((rule, input, expected), outcome) -> do
    putStrLn ("FAIL  " ++ render rule ++ "  on  " ++ render input)
    putStrLn ("      expected " ++ render expected)
    putStrLn ("      got      " ++ either show render outcome)
  putStrLn
    ( show (length cases - length failures)
        ++ " of "
        ++ show (length cases)
        ++ " cases pass; "
        ++ show (length entries - length cases)
        ++ " other entries skipped (comments)."
    )
  unless (null failures) exitFailure

render :: Value -> String
render = Char8.unpack . encode
