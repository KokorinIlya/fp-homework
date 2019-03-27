module Block2Spec
  ( evalSpec
  , movingAverageSpec
  ) where

import Test.Hspec (SpecWith, describe, it, shouldBe)
import Test.QuickCheck (property)

import Block2 (ArithmeticError (..), Expression (..), eval, moving)

movingAverageSpec :: SpecWith ()
movingAverageSpec =
  describe "Block2.moving" $ do
    it "calculates simple example" $ do
      moving 4 [1, 5, 3, 8, 7, 9, 6] `shouldBe` [1.0, 3.0, 3.0, 4.25, 5.75, 6.75, 7.5]

    it "calculates another simple example" $ do
      moving 2 [1, 5, 3, 8, 7, 9, 6] `shouldBe` [1.0, 3.0, 4.0, 5.5, 7.5, 8.0, 7.5]

    it "doen't change list, when window size is 1" $ do
      property checkWindowSize1

  where
    checkWindowSize1 :: [Double] -> Bool
    checkWindowSize1 x = moving 1 x == x

evalSpec :: SpecWith ()
evalSpec =
  describe "Block2.eval" $ do
    it "calculates const examples" $ do
      property checkConst

    it "calculates +" $ do
      property checkPlus

    it "calculates -" $ do
      property checkMinus

    it "calculates *" $ do
      property checkMul

    it "calculates /" $ do
      property checkDiv

    it "calculates ^" $ do
      property checkPow

    it "calculates correct examples" $ do
      eval (Division (Const 10) (Addition (Const 3) (Negation (Const (-2))))) `shouldBe` Right 2

    it "calculates another examples" $ do
      eval (Addition (Subtraction (Const 5) (Const 6)) (Multiplication (Const 2) (Const 3))) `shouldBe` Right 5

    it "fails with correct error, when division by zero is present" $ do
      eval (Division (Const 10) (Addition (Const 3) (Negation (Const 3)))) `shouldBe` Left DivisionByZero

    it "fails with correct error, when pow by negative number is present" $ do
      eval (Power (Const 10) (Addition (Const 3) (Negation (Const 4)))) `shouldBe` Left NegativePow

  where
    checkConst :: Int -> Bool
    checkConst x = eval (Const x) == Right x

    checkPlus :: Int -> Int -> Bool
    checkPlus x y = eval (Addition (Const x) (Const y)) == Right (x + y)

    checkMinus :: Int -> Int -> Bool
    checkMinus x y = eval (Subtraction (Const x) (Const y)) == Right (x - y)

    checkMul :: Int -> Int -> Bool
    checkMul x y = eval (Multiplication (Const x) (Const y)) == Right (x * y)

    checkDiv :: Int -> Int -> Bool
    checkDiv x y
      | y == 0    = eval (Division (Const x) (Const y)) == Left DivisionByZero
      | otherwise = eval (Division (Const x) (Const y)) == Right (x `div` y)

    checkPow :: Int -> Int -> Bool
    checkPow x y
      | y < 0     = eval (Power (Const x) (Const y)) == Left NegativePow
      | otherwise = eval (Power (Const x) (Const y)) == Right (x ^ y)
