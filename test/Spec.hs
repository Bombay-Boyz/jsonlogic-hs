module Main (main) where

import Test.Hspec (hspec)

import qualified EvalSpec
import qualified ParseSpec
import qualified ValueSpec

main :: IO ()
main = hspec $ do
  ParseSpec.spec
  ValueSpec.spec
  EvalSpec.spec
