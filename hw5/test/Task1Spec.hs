module Task1Spec
  ( stSpec
  ) where

import Task1 (fib, otherFib)
import Test.Hspec (SpecWith, describe, it, shouldBe)

stSpec :: SpecWith ()
stSpec =
  describe "Task1.ST" $ do
    it "calculates fibbonacci numbers" $ do
      fib 11 `shouldBe` 89
      otherFib 11 `shouldBe` 89
