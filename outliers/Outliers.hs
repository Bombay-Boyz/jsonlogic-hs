{-# LANGUAGE OverloadedStrings #-}

-- |
-- Replays the outlier fixture (test/outliers.jsonl) offline.
--
-- The fixture is written by differential/outliers.js from the reference
-- implementation's own answers; see that file for how the cases are built.
-- This runner needs neither Node nor a network, and it cannot pass by
-- running nothing: a missing, unreadable, truncated or empty fixture, an
-- operator with too few cases, or a line it cannot classify all fail the
-- run.
--
-- Every case is run twice:
--
--   * in 'Lenient' mode, where the library must agree with the reference
--     exactly: the same value, or an error of the same class; and
--   * in 'Strict' mode (the default), where it must give the same answer
--     unless the rule has an operator with the wrong number of arguments,
--     in which case it must reject it with 'ArityMismatch'.
--
-- Two documented deviations are honoured, each only for cases the
-- generator tagged (docs/COMPATIBILITY.md): "inherited" cases are held to
-- the reference's answer for a property that is certainly absent, and
-- "reeval" cases are counted but not compared.
--
-- Usage: jsonlogic-hs-outliers [FIXTURE]   (default test/outliers.jsonl)
module Main (main) where

import Control.Monad (unless, when)
import Data.Aeson
  ( Value (..)
  , eitherDecodeStrict
  , encode
  )
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Data.IORef
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, hSetEncoding, stderr, stdout, utf8)

import JsonLogic
import JsonLogic.Core.Operator (allBuiltins, builtinName)

-- | One fixture case.
data Case = Case
  { caseId :: Text
  , caseFam :: Text
  , caseOp :: Text
  , caseRule :: Value
  , caseData :: Value
  , caseExpect :: Reference
  , caseDev :: Maybe Text
  , caseOwn :: Maybe Reference
  }

data Reference = RefValue Value | RefError Text

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  args <- getArgs
  let path = case args of
        p : _ -> p
        [] -> "test/outliers.jsonl"
  exists <- doesFileExist path
  unless exists (die' ("outlier fixture not found: " ++ path))
  contents <- B.readFile path
  (meta, rows) <- case B.lines contents of
    metaLine : rest -> do
      declared <- either (die' . ("unreadable meta line: " ++)) pure (eitherDecodeStrict metaLine)
      pure (declared, rest)
    [] -> die' ("outlier fixture is empty: " ++ path)
  expected <- declaredCases meta
  when (length rows /= expected) $
    die' ("fixture has " ++ show (length rows) ++ " cases but declares " ++ show expected ++ ": truncated or edited")
  when (expected < 1000) $ die' "fixture declares fewer than 1000 cases: refusing to pass"
  cases <- mapM parseCase (zip [2 :: Int ..] rows)

  failures <- newIORef ([] :: [String])
  tally <- newIORef (Map.empty :: Map.Map String Int)
  let bump key = modifyIORef' tally (Map.insertWith (+) key 1)
      failWith msg = modifyIORef' failures (msg :)
  mapM_ (runCase bump failWith) cases

  -- coverage gate: every builtin operator, and every family, has cases
  let perOp = Map.fromListWith (+) [(Text.unpack (caseOp c), 1 :: Int) | c <- cases]
      perFam = Map.fromListWith (+) [(Text.unpack (caseFam c), 1 :: Int) | c <- cases]
  mapM_
    ( \o ->
        when (Map.findWithDefault 0 o perOp < 20) $
          failWith ("coverage: operator " ++ show o ++ " has fewer than 20 outlier cases")
    )
    builtins
  mapM_
    ( \f ->
        when (Map.findWithDefault 0 f perFam < 1) $
          failWith ("coverage: family " ++ show f ++ " has no cases")
    )
    families

  counts <- readIORef tally
  let count k = Map.findWithDefault 0 k counts
  putStrLn
    ( "outliers: "
        ++ show (length cases)
        ++ " cases from "
        ++ path
        ++ "\n  lenient agrees with the reference: "
        ++ show (count "lenient-ok")
        ++ "\n  strict agrees with the reference:  "
        ++ show (count "strict-ok")
        ++ "\n  strict rejected by arity:          "
        ++ show (count "strict-arity")
        ++ "\n  inherited-property deviation:      "
        ++ show (count "inherited")
        ++ "\n  re-evaluation deviation (skipped): "
        ++ show (count "reeval")
    )
  found <- readIORef failures
  unless (null found) $ do
    mapM_ (hPutStrLn stderr) (take 40 (reverse found))
    hPutStrLn stderr (show (length found) ++ " failures")
    exitFailure

declaredCases :: Value -> IO Int
declaredCases (Object o)
  | Just (Object m) <- KeyMap.lookup "meta" o
  , Just (Number n) <- KeyMap.lookup "cases" m
  , Just k <- toBoundedInteger n =
      pure k
declaredCases _ = die' "first line of the fixture is not a meta line"

die' :: String -> IO a
die' message = hPutStrLn stderr ("outliers: " ++ message) >> exitFailure

parseCase :: (Int, B.ByteString) -> IO Case
parseCase (line, bytes) = case eitherDecodeStrict bytes of
  Left reason -> die' ("line " ++ show line ++ ": " ++ reason)
  Right (Object o) -> do
    let text k = case KeyMap.lookup k o of
          Just (String t) -> pure t
          _ -> die' ("line " ++ show line ++ ": missing " ++ show k)
        field k = maybe (die' ("line " ++ show line ++ ": missing " ++ show k)) pure (KeyMap.lookup k o)
    i <- text "id"
    f <- text "fam"
    o' <- text "op"
    r <- field "rule"
    d <- field "data"
    e <- field "expect" >>= reference line
    own <- maybe (pure Nothing) (fmap Just . reference line) (KeyMap.lookup "expect_own" o)
    let dev = case KeyMap.lookup "dev" o of
          Just (String t) -> Just t
          _ -> Nothing
    pure (Case i f o' r d e dev own)
  Right _ -> die' ("line " ++ show line ++ ": not an object")

reference :: Int -> Value -> IO Reference
reference _ (Object o)
  | Just v <- KeyMap.lookup "ok" o = pure (RefValue v)
  | Just (String m) <- KeyMap.lookup "error" o = pure (RefError m)
reference line _ = die' ("line " ++ show line ++ ": bad expectation")

-- | Every builtin operator, from the library's own table.
builtins :: [String]
builtins = map (Text.unpack . builtinName) allBuiltins

families :: [String]
families =
  [ "arity"
  , "unary"
  , "pair"
  , "triple"
  , "numstr"
  , "numfmt"
  , "tie"
  , "signedzero"
  , "unicode"
  , "var"
  , "collection"
  , "flow"
  , "literal"
  , "deep"
  , "wide"
  , "coercion"
  , "community"
  ]

runCase :: (String -> IO ()) -> (String -> IO ()) -> Case -> IO ()
runCase bump failWith c
  | caseDev c == Just "reeval" = bump "reeval"
  | otherwise = do
      let target = case (caseDev c, caseOwn c) of
            (Just "inherited", Just own) -> own
            _ -> caseExpect c
          lenient = applyWithConfig defaultConfig{arityMode = Lenient} noOperators (caseRule c) (caseData c)
          strict = applyWithConfig defaultConfig noOperators (caseRule c) (caseData c)
      case judge target lenient of
        Nothing -> bump (if caseDev c == Just "inherited" then "inherited" else "lenient-ok")
        Just why -> failWith (describe c "lenient" target lenient why)
      case strict of
        Left (ParseFailure _ (ArityMismatch (OperatorName name) accepted given))
          | strictRejects c name accepted given -> bump "strict-arity"
          | otherwise ->
              failWith (describe c "strict" target strict "arity rejection that the operator's arity does not justify")
        _ -> case judge target strict of
          Nothing -> bump "strict-ok"
          Just why -> failWith (describe c "strict" target strict why)

-- | Is a strict-mode arity rejection justified: is the argument count
-- really outside what the operator takes?
strictRejects :: Case -> Text -> Arity -> Int -> Bool
strictRejects c _ accepted given = caseFam c `elem` ["flow", "arity", "unary", "pair", "triple", "collection", "community"] && outside
  where
    outside = case accepted of
      Exactly k -> given /= k
      AtLeast k -> given < k
      Between lo hi -> given < lo || given > hi

judge :: Reference -> Either Error Value -> Maybe String
judge (RefValue want) (Right got)
  | want == got = Nothing
  | otherwise = Just "different value"
judge (RefValue _) (Left e) = Just ("library failed: " ++ Text.unpack (renderError e))
judge (RefError _) (Right _) = Just "reference throws but the library returned a value"
judge (RefError message) (Left e)
  | classOf message e = Nothing
  | otherwise = Just ("error of a different class: " ++ Text.unpack (renderError e))

-- | The reference's error, by what it was, against the library's.
classOf :: Text -> Error -> Bool
classOf message e = case e of
  EvalFailure _ (UnknownOperator _) ->
    "Unrecognized operation" `isInfixOf` m || "apply is not a function" `isInfixOf` m
  EvalFailure _ (InvalidOperand (OperatorName "*")) -> "Reduce of empty array" `isInfixOf` m
  EvalFailure _ (InvalidOperand _) -> "Cannot read properties" `isInfixOf` m
  ParseFailure _ (ArityMismatch (OperatorName "*") _ 0) -> "Reduce of empty array" `isInfixOf` m
  _ -> False
  where
    m = Text.unpack message

describe :: Case -> String -> Reference -> Either Error Value -> String -> String
describe c mode target got why =
  concat
    [ Text.unpack (caseId c)
    , " ["
    , mode
    , "] "
    , why
    , "\n    rule:      "
    , L.unpack (encode (caseRule c))
    , "\n    data:      "
    , L.unpack (encode (caseData c))
    , "\n    reference: "
    , showRef target
    , "\n    library:   "
    , either (Text.unpack . renderError) (L.unpack . encode) got
    ]

showRef :: Reference -> String
showRef (RefValue v) = L.unpack (encode v)
showRef (RefError m) = "throws: " ++ Text.unpack m
