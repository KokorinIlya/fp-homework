module Task3Spec where

import Task3 (gauss)
import Test.Hspec (SpecWith, describe, it, shouldBe)

gaussSpec :: SpecWith ()
gaussSpec =
  describe "Task3.gauss" $ do
    it "solves systems" $ do
      gauss [[True, True, False], [False, False, False], [True, False, False]] [False, True, True] `shouldBe` Nothing
    it "solves systems" $ do
      gauss [[True, True, False], [False, False, False], [True, False, False]] [False, False, True] `shouldBe`
        Just [True, True, False]
    it "solves systems" $ do
      gauss [[True, True, False], [True, False, True], [False, True, True]] [True, True, False] `shouldBe`
        Just [True, False, False]
    it "solves systems" $ do
      gauss [[True, True, False], [True, False, True], [False, True, True]] [True, True, True] `shouldBe` Nothing
    it "solves systems" $ do
      gauss [[True, True], [True, True], [True, False]] [False, False, True] `shouldBe` Just [True, True]
    it "solves systems" $ do
     gauss [[True, False], [False, True], [True, True]] [True, True, True] `shouldBe` Nothing
    it "solves systems" $ do
      gauss [[True, False], [False, True], [True, True]] [True, True, False] `shouldBe` Just [True, True]
    it "solves systems" $ do
      gauss [[True, True, False, True], [True, False, False, True], [False, False, True, False]] [True, False, True] `shouldBe`
        Just [False, True, True, False]
