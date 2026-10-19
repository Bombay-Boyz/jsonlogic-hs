module Main (main) where

import Test.Hspec (hspec)

import qualified CompatSpec
import qualified EvalSpec
import qualified LimitsSpec
import qualified ParseSpec
import qualified ValueSpec

main :: IO ()
main = hspec $ do
  ParseSpec.spec
  ValueSpec.spec
  EvalSpec.spec
  CompatSpec.spec
  LimitsSpec.spec
