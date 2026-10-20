{-# LANGUAGE OverloadedStrings #-}

-- |
-- A downstream program that uses every documented way in: the simple
-- 'apply', the configurable 'applyWithConfig', compile-once-run-many, custom
-- operators and their refusals, strict and lenient arity, and a resource
-- limit. It imports only "JsonLogic", never a @JsonLogic.Core@ module, and
-- exits non-zero if any answer is not the expected one.
module Main (main) where

import Data.Aeson (Value (..), decode)
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.Maybe (fromMaybe)
import System.Exit (exitFailure)

import JsonLogic

-- | A JSON literal in this program; a malformed one becomes 'Null', which
-- makes the check that uses it fail.
json :: String -> Value
json text = fromMaybe Null (decode (Char8.pack text))

doubling :: Either OperatorsError Operators
doubling = operatorsFromList [(OperatorName "double", implementation)]
  where
    implementation _ [Number n] = Right (Number (n * 2))
    implementation _ _ = Left "expected one number"

checks :: [(String, Bool)]
checks =
  [
    ( "simple apply"
    , apply (json "{\"==\":[{\"var\":\"a\"},1]}") (json "{\"a\":1}") == Right (Bool True)
    )
  ,
    ( "compile once, run many"
    , (compile defaultConfig (json "{\"+\":[{\"var\":\"x\"},1]}") >>= \compiled -> traverse (run noOperators compiled) [json "{\"x\":1}", json "{\"x\":2}"])
        == Right [Number 2, Number 3]
    )
  ,
    ( "a step limit is a typed error"
    , applyWithConfig defaultConfig{maxSteps = 2} noOperators (json "{\"+\":[1,1]}") Null
        == Left (EvalFailure (LimitExceeded (StepLimit 2)))
    )
  ,
    ( "strict arity names the operator"
    , apply (json "{\"==\":[1]}") Null
        == Left (ParseFailure (ArityMismatch (OperatorName "==") (Exactly 2) 1))
    )
  ,
    ( "lenient arity follows the reference"
    , applyWithConfig defaultConfig{arityMode = Lenient} noOperators (json "{\"==\":[1]}") Null == Right (Bool False)
    )
  ,
    ( "a custom operator runs"
    , either (const False) (\ops -> applyWith ops (json "{\"double\":[21]}") Null == Right (Number 42)) doubling
    )
  ,
    ( "an unknown operator is an error, not null"
    , apply (json "{\"nope\":[1]}") Null == Left (EvalFailure (UnknownOperator (OperatorName "nope")))
    )
  ,
    ( "a custom operator may not shadow a builtin"
    , either Just (const Nothing) (operatorsFromList [(OperatorName "var", \_ _ -> Right Null)])
        == Just (ReservedOperatorName (OperatorName "var"))
    )
  ]

main :: IO ()
main = do
  let failed = [name | (name, ok) <- checks, not ok]
  mapM_ (putStrLn . ("FAIL: " ++)) failed
  if null failed
    then putStrLn ("consumer smoke test: all " ++ show (length checks) ++ " checks passed")
    else exitFailure
