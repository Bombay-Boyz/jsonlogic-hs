module Main (main) where

import Test.Hspec (hspec)

import qualified AnalysisSpec
import qualified CompatSpec
import qualified EvalSpec
import qualified LimitsSpec
import qualified LocationSpec
import qualified NumberSpec
import qualified ParseSpec
import qualified RegressionSpec
import qualified ValueSpec

main :: IO ()
main = hspec $ do
  ParseSpec.spec
  NumberSpec.spec
  ValueSpec.spec
  EvalSpec.spec
  CompatSpec.spec
  LimitsSpec.spec
  LocationSpec.spec
  AnalysisSpec.spec
  RegressionSpec.spec
