{-# LANGUAGE OverloadedStrings #-}

-- |
-- Benchmarks (Standard 5.6): statistically sound timing and allocation of
-- the engine, by tasty-bench.
--
-- Four groups, so each cost is read on its own:
--
-- * @one-shot@: 'apply', which reads the rule and evaluates it, the cost of
--   a rule used once.
-- * @compile only@: reading a rule, with no evaluation.
-- * @precompiled run@: 'run' of a rule read beforehand, the cost of a rule
--   used many times. One-shot minus this is the reading cost.
-- * @limits@: a rule that stops at a resource limit.
--
-- Large inputs live in the /data/, built once, and every rule or datum
-- depends on the argument tasty-bench passes in, so GHC cannot lift a
-- benchmark's work out of the timing loop and run it only once.
--
-- > cabal run -O2 jsonlogic-hs-bench -- --csv results.csv
-- > cabal run -O2 jsonlogic-hs-bench -- --baseline old.csv --fail-if-slower 15
--
-- 'scripts/bench.sh' and 'scripts/bench-compare.sh' wrap both, and record
-- the environment next to the numbers. Timings are comparable only between
-- runs on one machine; see bench/README.md.
module Main (main) where

import Data.Aeson (Value (..), encode, object, toJSON, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Lazy as LBS
import Data.Either (fromRight)
import qualified Data.Text as Text
import Test.Tasty.Bench (Benchmark, bench, bgroup, defaultMain, whnf)

import JsonLogic

-- | The size of the encoded answer: forces the whole result, and gives the
-- benchmark an 'Int' to hold on to.
size :: Either Error Value -> Int
size = fromIntegral . LBS.length . encode . fromRight Null

num :: Int -> Value
num = toJSON

stringOf :: Int -> Value
stringOf = String . Text.pack . show

var :: Text.Text -> Value
var path = object ["var" .= path]

call :: Text.Text -> [Value] -> Value
call operator operands = object [Key.fromText operator .= operands]

-- | 10,000 numbers, the size the iteration benchmarks run over.
bigNumbers :: Value
bigNumbers = toJSON [1 .. 10000 :: Int]

-- | The data every workload is evaluated against. Parts a rule does not
-- touch are never converted, so a small rule pays nothing for the big array.
dataFor :: Int -> Value
dataFor n =
  object
    [ "x" .= n
    , "xs" .= bigNumbers
    , "a" .= object ["b" .= object ["c" .= object ["d" .= n]]]
    ]

-- | One custom operator, to price the extension point.
customOps :: Operators
customOps = fromRight noOperators (operatorsFromList [(OperatorName "double", doubling)])
  where
    doubling _ [Number k] = Right (Number (k * 2))
    doubling _ _ = Left "expected one number"

-- ---------------------------------------------------------------------
-- Workloads: each is a rule built from the benchmark's argument
-- ---------------------------------------------------------------------

workloads :: [(String, Int -> Value)]
workloads =
  [ ("arithmetic: x + n", \n -> call "+" [var "x", num n])
  , ("short-circuit: and, false first, 1,000 operands", \n -> call "and" (Bool False : num n : replicate 1000 (var "x")))
  , ("nested if, depth 60", \n -> foldr (\_ inner -> call "if" [Bool True, inner, num 0]) (num n) [1 .. 60 :: Int])
  , ("var path a.b.c.d", \_ -> var "a.b.c.d")
  , ("var 50 lookups to the end of a 10,000-element array", \n -> call "+" (num n : replicate 50 (var "xs.9999")))
  , ("map over 10,000 elements", \n -> call "map" [var "xs", call "*" [var "", num n]])
  , ("filter over 10,000 elements", \n -> call "filter" [var "xs", call ">" [var "", num (n + 5000)]])
  , ("reduce (sum) over 10,000 elements", \n -> call "reduce" [var "xs", call "+" [var "current", var "accumulator"], num n])
  , ("cat of 1,000 short strings", call "cat" . replicate 1000 . stringOf)
  , ("number printing via cat, 1,000 doubles", \n -> call "cat" (map (\i -> toJSON (fromIntegral (i + n) / 7 :: Double)) [1 .. 1000 :: Int]))
  , ("string to number, 200-digit literal", \n -> call "+" [String (Text.pack (show n) <> Text.replicate 200 "7")])
  , ("custom operator, composed twice", \n -> call "double" [call "double" [num n]])
  ]

-- ---------------------------------------------------------------------
-- The four groups
-- ---------------------------------------------------------------------

oneShot :: (String, Int -> Value) -> Benchmark
oneShot (name, rule) =
  bench name (whnf (\n -> size (applyWith customOps (rule n) (dataFor n))) 1)

verdict :: Either Error Rule -> Int
verdict = either (const 0) (const 1)

compileOnly :: (String, Int -> Value) -> Benchmark
compileOnly (name, rule) =
  bench name (whnf (verdict . compile defaultConfig . rule) 1)

-- | The rule is read once, outside the timing loop; only 'run' is timed.
precompiledRun :: (String, Int -> Value) -> Benchmark
precompiledRun (name, rule) =
  bench name (whnf (\n -> maybe 0 (\r -> size (run customOps r (dataFor n))) compiled) 1)
  where
    compiled = either (const Nothing) Just (compile defaultConfig (rule 0))

-- | 110,000 operands against a limit of 100,000 steps: the cost of being
-- refused.
limitHit :: Benchmark
limitHit =
  bench "step limit reached, 110,000 operands" $
    whnf (\n -> size (applyWithConfig defaultConfig{maxSteps = 100000} customOps (call "+" (num n : replicate 110000 (num 1))) Null)) 1

main :: IO ()
main =
  defaultMain
    [ bgroup "one-shot" (map oneShot workloads)
    , bgroup "compile only" (map compileOnly workloads)
    , bgroup "precompiled run" (map precompiledRun workloads)
    , bgroup "limits" [limitHit]
    ]
