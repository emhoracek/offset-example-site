module Main where

import Test.Hspec

main = hspec $ do
  describe "pending" $
    it "is pending" $
      1 `shouldBe` 1