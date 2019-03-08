module Task1Spec
  ( stringSumSpec
  ) where

import Test.Hspec (describe, it, SpecWith, shouldBe)
import Test.QuickCheck (property)
import Task1 (stringSum)

stringSumSpec :: SpecWith ()
stringSumSpec =
  describe "Task1.stringSum" $ do
    it "calculates sum, when there are only numbers in string" $ do
      ((stringSum "1 2 3") :: Maybe Int) `shouldBe` (Just 6)

    it "calculates sum of doubles" $ do
      ((stringSum "1.0 2.2 3.5") :: Maybe Double) `shouldBe` (Just 6.7)

    it "fails, when non only numbers appear in the string" $ do
      ((stringSum "1 2 a") :: Maybe Int) `shouldBe` Nothing

    it "parses numbers, separated by any types of spaces" $ do
      ((stringSum "1 \n 2 \n\n\n\t 3 \t\t\t\t\n\n\n    \n\t   4") :: Maybe Int) `shouldBe` (Just 10)

    it "Parces any correct input" $ do
      property correctInputChecker

  where
    correctInputChecker :: [Int] -> Bool
    correctInputChecker numbers =
      let stringWithNumbers = foldr (\number acc -> show number ++ " " ++ acc) "" numbers in
      stringSum stringWithNumbers == Just (sum numbers)


