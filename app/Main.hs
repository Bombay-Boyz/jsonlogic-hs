{-# LANGUAGE OverloadedStrings #-}

-- |
-- The @jsonlogic@ command: try, check and inspect rules without writing
-- Haskell.
--
-- > jsonlogic eval  RULE [DATA] [options]   apply a rule to data (default null)
-- > jsonlogic check RULE        [options]   report every problem in a rule
-- > jsonlogic uses  RULE        [options]   print the data paths a rule reads
--
-- RULE and DATA are files of JSON, or @-@ for standard input. The result
-- of @eval@ is written to standard output as JSON. Exit status: 0 success,
-- 1 the rule was refused or failed, 2 bad usage or input that is not JSON.
module Main (main) where

import Control.Monad (forM_, when)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.List (isPrefixOf)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr, stdin)
import Text.Read (readMaybe)

import JsonLogic

data Format = Text | JsonFormat
  deriving (Eq)

data Options = Options
  { optConfig :: Config
  , optFormat :: Format
  , optTimeout :: Maybe Int
  , optFiles :: [FilePath]
  }

usage :: String
usage =
  unlines
    [ "usage: jsonlogic eval  RULE [DATA] [options]"
    , "       jsonlogic check RULE        [options]"
    , "       jsonlogic uses  RULE        [options]"
    , ""
    , "RULE and DATA are JSON files, or - for standard input."
    , ""
    , "options:"
    , "  --lenient             the reference's arity rules; unknown operators fail only when reached"
    , "  --allow-unknown       unknown operators fail only when reached"
    , "  --max-steps N         evaluation step budget"
    , "  --max-depth N         rule nesting limit"
    , "  --preserve-numbers    keep the digits of numbers a double cannot hold, when passed through"
    , "  --reject-lossy        refuse numbers a double cannot hold"
    , "  --timeout MS          wall-clock limit for eval, in milliseconds"
    , "  --format text|json    how problems are reported (default text)"
    ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    command : rest | command `elem` ["eval", "check", "uses"] -> case parseOptions rest of
      Left problem -> bad problem
      Right options -> dispatch command options
    _ -> bad "expected a command: eval, check or uses"

bad :: String -> IO a
bad message = hPutStrLn stderr ("jsonlogic: " ++ message) >> hPutStrLn stderr usage >> exitWith (ExitFailure 2)

parseOptions :: [String] -> Either String Options
parseOptions = go (Options defaultConfig Text Nothing [])
  where
    go options [] = Right options {optFiles = reverse (optFiles options)}
    go options (flag : rest) = case flag of
      "--lenient" -> go (setConfig options (\c -> c {arityMode = Lenient})) rest
      "--allow-unknown" -> go (setConfig options (\c -> c {unknownOperators = FailWhenReached})) rest
      "--preserve-numbers" -> go (setConfig options (\c -> c {numericSafety = PreserveSource})) rest
      "--reject-lossy" -> go (setConfig options (\c -> c {numericSafety = RejectLossy})) rest
      "--max-steps" -> number flag rest (\n -> setConfig options (\c -> c {maxSteps = n}))
      "--max-depth" -> number flag rest (\n -> setConfig options (\c -> c {maxDepth = n}))
      "--timeout" -> number flag rest (\n -> options {optTimeout = Just n})
      "--format" -> case rest of
        "text" : more -> go options {optFormat = Text} more
        "json" : more -> go options {optFormat = JsonFormat} more
        _ -> Left "--format takes text or json"
      _
        | "--" `isPrefixOf` flag -> Left ("unknown option " ++ flag)
        | otherwise -> go options {optFiles = flag : optFiles options} rest
    setConfig options f = options {optConfig = f (optConfig options)}
    number flag rest build = case rest of
      value : more | Just n <- readMaybe value, n >= 0 -> go (build n) more
      _ -> Left (flag ++ " takes a non-negative number")

dispatch :: String -> Options -> IO ()
dispatch command options = case (command, optFiles options) of
  ("eval", [ruleFile]) -> eval options ruleFile Nothing
  ("eval", [ruleFile, dataFile]) -> eval options ruleFile (Just dataFile)
  ("check", [ruleFile]) -> check options ruleFile
  ("uses", [ruleFile]) -> uses options ruleFile
  _ -> bad ("wrong number of files for " ++ command)

readJson :: String -> FilePath -> IO Aeson.Value
readJson what path = do
  bytes <- if path == "-" then LBS.hGetContents stdin else LBS.readFile path
  case Aeson.eitherDecode bytes of
    Left problem -> hPutStrLn stderr ("jsonlogic: " ++ what ++ " is not JSON: " ++ problem) >> exitWith (ExitFailure 2)
    Right value -> pure value

report :: Options -> [Error] -> IO a
report options problems = do
  case optFormat options of
    Text -> forM_ problems (Text.IO.hPutStrLn stderr . renderError)
    JsonFormat -> Char8.hPutStrLn stderr (Aeson.encode problems)
  exitWith (ExitFailure 1)

eval :: Options -> FilePath -> Maybe FilePath -> IO ()
eval options ruleFile dataFile = do
  rule <- readJson "the rule" ruleFile
  input <- maybe (pure Aeson.Null) (readJson "the data") dataFile
  ready <- either (report options . pure) pure (compile (optConfig options) noOperators rule)
  when (numericSafety (optConfig options) == RejectLossy) $
    either (report options . pure) pure (checkData (optConfig options) input)
  outcome <- case optTimeout options of
    Just milliseconds -> runWithTimeout (milliseconds * 1000) ready input
    Nothing -> tryRun ready input
  case outcome of
    Right result -> Char8.putStrLn (Aeson.encode result)
    Left (Rejected problem) -> report options [problem]
    Left TimedOut -> hPutStrLn stderr "jsonlogic: timed out" >> exitWith (ExitFailure 1)
    Left (OperatorThrew e) -> hPutStrLn stderr ("jsonlogic: " ++ show e) >> exitWith (ExitFailure 1)

check :: Options -> FilePath -> IO ()
check options ruleFile = do
  rule <- readJson "the rule" ruleFile
  case validate (optConfig options) noOperators rule of
    [] -> pure ()
    problems -> report options problems

uses :: Options -> FilePath -> IO ()
uses options ruleFile = do
  rule <- readJson "the rule" ruleFile
  ready <- either (report options . pure) pure (compile (optConfig options) noOperators rule)
  let use = dataUse ready
  if usesAllData use
    then putStrLn "(all of the data)"
    else forM_ (usedPaths use) (putStrLn . Text.unpack)
