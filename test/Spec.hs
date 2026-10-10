module Main (main) where

import Test.Hspec

main :: IO ()
main = hspec $
  describe "Phase 0 scaffold" $
    it "compiles and runs, proving the toolchain is wired" $
      True `shouldBe` True
