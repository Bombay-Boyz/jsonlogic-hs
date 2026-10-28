{-# LANGUAGE OverloadedStrings #-}

-- |
-- Benchmarks (Standard 5.6): statistically sound timing and allocation of
-- the engine, by tasty-bench.
--
-- Five groups, so each cost is read on its own:
--
-- * @one-shot@: 'apply', which reads the rule and evaluates it, the cost of
--   a rule used once.
-- * @compile only@: reading a rule and building all of its code, with no
--   evaluation.
-- * @precompiled run@: 'run' of a rule read beforehand, the cost of a rule
--   used many times. One-shot minus this is the reading cost.
-- * @limits@: a rule that stops at a resource limit, and is refused before
--   it is read.
-- * @large inputs@: the shapes that were once able to do far more work than
--   the step budget showed: long strings, huge numbers, wide rules, a custom
--   operator over a large datum. Each is held to its budget here, so a
--   regression shows as time.
--
-- Every result is forced with 'nf', not by encoding it: what is timed is
-- the engine, not the JSON encoder.
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

import Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.Aeson.Key as Key
import Data.Either (fromRight)
import Data.Scientific (scientific)
import qualified Data.Text as Text
import Test.Tasty.Bench (Benchmark, bench, bgroup, defaultMain, nf)

import JsonLogic

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
  bench name (nf (\n -> applyWith customOps (rule n) (dataFor n)) 1)

-- | 'compile' builds all of a rule's code, so forcing it times all of that.
compileOnly :: (String, Int -> Value) -> Benchmark
compileOnly (name, rule) =
  bench name (nf (compile defaultConfig customOps . rule) 1)

-- | The rule is read once, outside the timing loop; only 'run' is timed.
precompiledRun :: (String, Int -> Value) -> Benchmark
precompiledRun (name, rule) =
  bench name (nf (\n -> fmap (\r -> run r (dataFor n)) compiled) 1)
  where
    compiled = compile defaultConfig customOps (rule 0)

-- | 110,000 operands against a limit of 100,000 steps: the cost of being
-- refused.
limitHit :: Benchmark
limitHit =
  bench "step limit reached, 110,000 operands" $
    nf (\n -> applyWithConfig defaultConfig{maxSteps = 100000} customOps (call "+" (num n : replicate 110000 (num 1))) Null) 1

-- | Twice the node limit: refused before any of it is read.
nodeLimitHit :: Benchmark
nodeLimitHit =
  bench "node limit reached, 2,000,000 operands" $
    nf (\n -> applyWith customOps (call "+" (num n : replicate 2000000 (num 1))) Null) 1

-- ---------------------------------------------------------------------
-- Large inputs
-- ---------------------------------------------------------------------

-- | A 1,000,000-character string, built once.
bigString :: Value
bigString = String (Text.replicate 1000000 "a")

-- | A 100,000-key object, built once.
bigObject :: Value
bigObject = object [Key.fromText (Text.pack ("k" ++ show i)) .= i | i <- [1 .. 100000 :: Int]]

-- | A decimal number of 200,000 digits, built once.
bigNumber :: Value
bigNumber = Number (scientific (10 ^ (200000 :: Int) + 12345) (-200000))

-- | Operators that read, compare and search a 1,000,000-character string, a
-- number of times each: far more than the step budget allows for the long
-- ones, so these are timed refusals, and the one-use rows are timed work.
largeInputs :: [(String, Int -> (Value, Value))]
largeInputs =
  [ ("one comparison of two 1M-character strings", \n -> (call "<" [var "big", var "big"], withBig n))
  , ("one substr of a 1M-character string", \n -> (call "substr" [var "big", num 0, num 1], withBig n))
  , ("one search of a 1M-character string", \n -> (call "in" ["b", var "big"], withBig n))
  , ("1,000 comparisons of 1M-character strings, refused", \n -> (call "+" (replicate 1000 (call "<" [var "big", var "big"])), withBig n))
  , ("a 200,000-digit JSON number read", \n -> (call "+" [var "number", num n], withBig n))
  , ("custom operator, 200 calls over a 100,000-key datum", \n -> (call "+" (replicate 200 (call "double" [num n])), bigObject))
  , ("100,000-operand rule", \n -> (call "+" (num n : replicate 100000 (num 1)), Null))
  ]
  where
    withBig n = object ["big" .= bigString, "number" .= bigNumber, "n" .= n]

largeInput :: (String, Int -> (Value, Value)) -> Benchmark
largeInput (name, workload) =
  bench name (nf (\n -> let (rule, dat) = workload n in applyWith customOps rule dat) 1)

main :: IO ()
main =
  defaultMain
    [ bgroup "one-shot" (map oneShot workloads)
    , bgroup "compile only" (map compileOnly workloads)
    , bgroup "precompiled run" (map precompiledRun workloads)
    , bgroup "limits" [limitHit, nodeLimitHit]
    , bgroup "large inputs" (map largeInput largeInputs)
    ]
