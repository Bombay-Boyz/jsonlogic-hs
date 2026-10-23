{-# LANGUAGE OverloadedStrings #-}

-- |
-- Every example in docs/TUTORIAL.md, with the answer the tutorial shows. The
-- run fails if any answer differs, so the tutorial cannot drift from the
-- library. Uses only the public "JsonLogic" module, as a reader would.
module Main (main) where

import Data.Aeson (Value (..), decode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Exit (exitFailure)

import JsonLogic

json :: String -> Value
json = fromMaybe Null . decode . Char8.pack

startsWith :: CustomOperator
startsWith _ [String prefix, String s] = Right (Bool (prefix `Text.isPrefixOf` s))
startsWith _ _ = Left "starts_with takes two strings"

operators :: Either OperatorsError Operators
operators = operatorsFromList [(OperatorName "starts_with", startsWith)]

describe :: Either Error Value -> String
describe result = case result of
  Right v -> "ok: " ++ show v
  Left (ParseFailure at (ArityMismatch (OperatorName op) _ given)) ->
    "rule error at " ++ show (renderLocation at) ++ ": " ++ show op ++ " given " ++ show given
  Left (EvalFailure _ (LimitExceeded _)) -> "too expensive"
  Left other -> "failed: " ++ show (renderError other)

-- | (section, what the tutorial shows, what the library says)
checks :: [(String, String, String)]
checks =
  [ check "1 and" (Right (Bool True)) (apply (json "{\"and\": [{\">=\": [{\"var\": \"age\"}, 18]}, {\"var\": \"verified\"}]}") (json "{\"age\": 21, \"verified\": true}"))
  , check "1 array" (Right (json "[1,2]")) (apply (json "[{\"var\": \"a\"}, 2]") (json "{\"a\": 1}"))
  , check "1 path" (Right (String "admin")) (apply (json "{\"var\": \"user.roles.0\"}") (json "{\"user\": {\"roles\": [\"admin\"]}}"))
  , check "1 default" (Right (String "default")) (apply (json "{\"var\": [\"missing.path\", \"default\"]}") (json "{}"))
  , check "2 ==" (Right (Bool True)) (apply (json "{\"==\": [1, \"1\"]}") Null)
  , check "2 ===" (Right (Bool False)) (apply (json "{\"===\": [1, \"1\"]}") Null)
  , check "2 <" (Right (Bool True)) (apply (json "{\"<\": [\"10\", \"9\"]}") Null)
  , check "2 +" (Right (Number 7)) (apply (json "{\"+\": [\"3\", 4]}") Null)
  , check "2 /" (Right Null) (apply (json "{\"/\": [1, 0]}") Null)
  , check "2 cat" (Right (String "0.1+0.2")) (apply (json "{\"cat\": [0.1, \"+\", 0.2]}") Null)
  , check "2 if []" (Right (String "no")) (apply (json "{\"if\": [[], \"yes\", \"no\"]}") Null)
  , check "2 !!" (Right (Bool True)) (apply (json "{\"!!\": \"0\"}") Null)
  , check
      "3 compile"
      (Right [Right (Number 12), Right (Number 3)])
      ((\rule -> map (run rule) [json "{\"price\": 10}", json "{\"price\": 2.5}"]) <$> compile defaultConfig noOperators (json "{\"*\": [{\"var\": \"price\"}, 1.2]}"))
  , check
      "4 location"
      (Left (EvalFailure (fromSegments [Key "if", Index 1]) (UnknownOperator (OperatorName "nope"))))
      (apply (json "{\"if\": [true, {\"nope\": [1]}]}") Null)
  , check "4 render" ("at /if/1: unknown operator \"nope\"" :: Text) (either renderError (const "") (apply (json "{\"if\": [true, {\"nope\": [1]}]}") Null))
  , check "4 describe" "rule error at \"\": \"==\" given 1" (describe (apply (json "{\"==\": [1]}") Null))
  , check "4 lazy" (Right (Bool True)) (apply (json "{\"or\": [true, {\"nope\": []}]}") Null)
  , check
      "5 strict"
      (Left (ParseFailure (fromSegments []) (ArityMismatch (OperatorName "==") (Exactly 2) 1)))
      (apply (json "{\"==\": [1]}") Null)
  , check "5 lenient" (Right (Bool False)) (applyWithConfig defaultConfig{arityMode = Lenient} noOperators (json "{\"==\": [1]}") Null)
  , check
      "6 limits"
      (Left (EvalFailure (fromSegments [Key "map", Index 1]) (LimitExceeded (ArrayLimit 1000))))
      ( applyWithConfig
          defaultConfig{maxSteps = 10000, maxStringLength = 4096, maxArrayLength = 1000}
          noOperators
          (json "{\"map\": [{\"var\": \"xs\"}, {\"map\": [{\"var\": \"\"}, 1]}]}")
          (object ["xs" .= [[1 .. 2000 :: Int]]])
      )
  , check
      "7 custom"
      (Right (Right (Bool True)))
      ((\ops -> applyWith ops (json "{\"starts_with\": [\"ad\", {\"var\": \"role\"}]}") (json "{\"role\": \"admin\"}")) <$> operators)
  , check
      "7 custom failure"
      (Right (Left (EvalFailure (fromSegments []) (CustomOperatorFailed (OperatorName "starts_with") "starts_with takes two strings"))))
      ((\ops -> applyWith ops (json "{\"starts_with\": [1]}") Null) <$> operators)
  , check
      "8 dataUse"
      (Right (DataUse ["orders", "user.age"] False))
      ( dataUse
          <$> compile
            defaultConfig
            noOperators
            (json "{\"and\": [{\"var\": \"user.age\"}, {\"some\": [{\"var\": \"orders\"}, {\">\": [{\"var\": \"total\"}, 100]}]}]}")
      )
  ]
  where
    check :: (Eq a, Show a) => String -> a -> a -> (String, String, String)
    check name shown actual = (name, show shown, if actual == shown then show shown else show actual)

main :: IO ()
main = do
  let wrong = [(name, shown, actual) | (name, shown, actual) <- checks, shown /= actual]
  mapM_ (\(name, shown, actual) -> putStrLn ("tutorial section " ++ name ++ ":\n  shows " ++ shown ++ "\n  gives " ++ actual)) wrong
  if null wrong
    then putStrLn ("tutorial: all " ++ show (length checks) ++ " examples give the answers shown")
    else exitFailure
