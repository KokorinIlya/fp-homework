module Block1Spec
  ( stringSumSpec
  , treeSpec
  , nonEmptySpec
  ) where

import Test.Hspec (describe, it, SpecWith, shouldBe)
import Test.QuickCheck (property)
import Block1 (stringSum, Tree (..), NonEmpty (..))

stringSumSpec :: SpecWith ()
stringSumSpec =
  describe "Block1.stringSum" $ do
    it "calculates sum, when there are only numbers in string" $ do
      ((stringSum "1 2 3") :: Maybe Int) `shouldBe` (Just 6)

    it "calculates sum of doubles" $ do
      ((stringSum "1.0 2.2 3.5") :: Maybe Double) `shouldBe` (Just 6.7)

    it "fails, when not only numbers appear in the string" $ do
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

treeSpec :: SpecWith ()
treeSpec =
  describe "Block1.Tree" $ do
    it "calculates foldr from right to left" $ do
      let tree = Branch (Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3))) (Branch (Leaf 4) (Leaf 5))
       in (foldr (\cur acc -> acc * 10 + cur) 0 tree) `shouldBe` (54321 :: Int)

    it "calculates foldl from left to right" $ do
      let tree = Branch (Branch (Leaf 1) (Branch (Leaf 2) (Branch (Leaf 3) (Leaf 4)))) (Branch (Leaf 5) (Leaf 6))
       in (foldl (\acc cur -> acc * 10 + cur) 0 tree) `shouldBe` (123456 :: Int)

nonEmptySpec :: SpecWith ()
nonEmptySpec =
  describe "Block1.NonEmpty" $ do
    it "calculates fmap like non empty list" $ do
     property checkFmap

    -- сделать property, функции генерить через (+) <$> randomList
    it "calculates <*> like non empty list" $ do
      let xs :: [Integer]
          xs = [(+ 2), (* 3), (1 -), (`subtract` 4)] <*> [1, 3, 3, 7]
          y :: Integer
          ys :: [Integer]
          (y :| ys) = (+ 2) :| [(* 3), (1 -), (`subtract` 4)] <*> 1 :| [3, 3, 7]
       in (y : ys) `shouldBe` xs

    it "calculates >>= like non empty list" $ do
      let xs :: [Int]
          xs = [2, 5, 1, 7, 1, 3, 3, 7] >>= (\x -> replicate (x + 1) x)
          y :: Int
          ys :: [Int]
          (y :| ys) = 2 :| [5, 1, 7, 1, 3, 3, 7] >>= (\x -> x :| replicate x x)
       in (y : ys) `shouldBe` xs

    it "calculates foldr like foldr for non empty list" $ do
      foldr (\cur acc -> acc * 10 + cur) 0 (1 :| [2, 3, 4, 5]) `shouldBe` (54321 :: Int)

    it "calculates foldl like foldr for non empty list" $ do
      foldl (\acc cur -> acc * 10 + cur) 0 (1 :| [2, 3, 4, 5]) `shouldBe` (12345 :: Int)

  where
    checkFmap :: Int -> [Int] -> Bool
    checkFmap x xs = all (checkFmapForOne x xs) [(+ 2), (* 3), (1 -), (`subtract` 4)]

    checkFmapForOne :: Int -> [Int] -> (Int -> Int) -> Bool
    checkFmapForOne x xs f =
      let mappedList = fmap f (x : xs)
          (y :| ys) = fmap f (x :| xs)
       in (mappedList == y : ys)
