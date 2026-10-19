{-# LANGUAGE OverloadedStrings #-}

-- |
-- Benchmarks: a fixed set of workloads, each timed over several runs and
-- reported as the best time (the least noisy statistic for a CPU-bound
-- pure function) in microseconds per operation.
--
-- Usage:
--
-- > cabal run jsonlogic-hs-bench -- [REPEATS]
--
-- Output is one line per workload, @name  best-µs  per-op-µs@, followed by
-- the GC and allocation totals of the whole run. Compare two runs by hand
-- or with @scripts/bench-compare.sh@; there is no committed baseline,
-- because timings are only comparable on one machine.
--
-- The workloads cover the shapes that matter: a tiny rule applied many
-- times (the common case), compile-once versus apply, iteration over
-- large arrays, string building, number parsing and printing, deep
-- nesting, and a rule that hits a resource limit.
module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (forM_)
import Data.Aeson (Value (..), encode, object, toJSON, (.=))
import qualified Data.ByteString.Lazy as LBS
import Data.Either (fromRight)
import Data.List (minimumBy)
import Data.Ord (comparing)
import qualified Data.Text as Text
import GHC.Clock (getMonotonicTime)
import System.Environment (getArgs)

import JsonLogic

-- | A workload: its name, how many operations one run performs, and the
-- action that performs them and forces the result. The action is given a
-- different "salt" on every run and builds its input from it, so that GHC
-- cannot share one run's work with the next and time nothing.
data Workload = Workload String Int (Int -> IO ())

force :: Either Error Value -> IO ()
force result = do
  _ <- evaluate (LBS.length (encode (fromRight Null result)))
  pure ()

num :: Int -> Value
num = toJSON

numbers :: Int -> Int -> Value
numbers salt n = toJSON [salt .. salt + n - 1]

workloads :: [Workload]
workloads =
  [ Workload "small rule, apply (1000x)" 1000 $ \salt ->
      forM_ [1 .. 1000 :: Int] $ \i ->
        force (apply (object ["+" .= [object ["var" .= ("x" :: String)], num salt]]) (object ["x" .= num i]))
  , Workload "small rule, compile once then run (1000x)" 1000 $ \salt ->
      case compile defaultConfig (object ["+" .= [object ["var" .= ("x" :: String)], num salt]]) of
        Left _ -> pure ()
        Right rule -> forM_ [1 .. 1000 :: Int] $ \i -> force (run noOperators rule (object ["x" .= num i]))
  , Workload "map over 10,000 elements" 10000 $ \salt ->
      force (apply (object ["map" .= [numbers salt 10000, object ["*" .= [object ["var" .= ("" :: String)], num 2]]]]) Null)
  , Workload "filter over 10,000 elements" 10000 $ \salt ->
      force (apply (object ["filter" .= [numbers salt 10000, object [">" .= [object ["var" .= ("" :: String)], num (salt + 5000)]]]]) Null)
  , Workload "reduce (sum) over 10,000 elements" 10000 $ \salt ->
      force
        ( apply
            ( object
                [ "reduce"
                    .= [ numbers salt 10000
                       , object ["+" .= [object ["var" .= ("current" :: String)], object ["var" .= ("accumulator" :: String)]]]
                       , num 0
                       ]
                ]
            )
            Null
        )
  , Workload "cat of 1,000 short strings" 1000 $ \salt ->
      force (apply (object ["cat" .= replicate 1000 (String (Text.pack (show salt)))]) Null)
  , Workload "string to number, 200-digit literals (100x)" 100 $ \salt ->
      forM_ [1 .. 100 :: Int] $ \i ->
        force (apply (object ["+" .= [String (Text.pack (show (salt + i)) <> Text.replicate 200 "7")]]) Null)
  , Workload "number printing via cat (1,000 doubles)" 1000 $ \salt ->
      force (apply (object ["cat" .= map (\i -> toJSON (fromIntegral (i + salt) / 7 :: Double)) [1 .. 1000 :: Int]]) Null)
  , Workload "nested if, depth 60" 1 $ \salt ->
      force (apply (foldr (\_ inner -> object ["if" .= [Bool True, inner, num 0]]) (num salt) [1 .. 60 :: Int]) Null)
  , Workload "var path into nested data (1000x)" 1000 $ \salt ->
      forM_ [1 .. 1000 :: Int] $ \i ->
        force (apply (object ["var" .= ("a.b.c.d" :: String)]) (object ["a" .= object ["b" .= object ["c" .= object ["d" .= num (salt + i)]]]]))
  , Workload "rule that hits the step limit (1x)" 1 $ \salt ->
      force (apply (object ["+" .= (num salt : replicate 1100000 (num 1))]) Null)
  ]

main :: IO ()
main = do
  args <- getArgs
  let repeats = case args of
        n : _ | [(k, "")] <- reads n -> max 1 k
        _ -> 5
  putStrLn ("jsonlogic-hs benchmarks, best of " ++ show repeats ++ " runs")
  forM_ workloads $ \(Workload name ops action) -> do
    times <-
      mapM
        ( \salt -> do
            start <- getMonotonicTime
            action salt
            end <- getMonotonicTime
            pure (end - start)
        )
        [1 .. repeats]
    let best = minimumBy (comparing id) times
    putStrLn
      ( pad 48 name
          ++ pad 14 (show (round (best * 1e6) :: Int) ++ " us")
          ++ show (fromIntegral (round (best * 1e9 / fromIntegral ops) :: Int) / 1000 :: Double)
          ++ " us/op"
      )
  where
    pad n s = s ++ replicate (n - length s) ' '
