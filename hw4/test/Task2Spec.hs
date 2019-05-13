module Task2Spec
  ( doubleAreaSpec
  , doubleAreaSlowSpec
  , perimeterSpec
  , perimeterSlowSpec
  ) where

import Task2 (Point(..), doubleArea, doubleAreaSlow, perimeter, perimeterSlow)
import Test.Hspec (SpecWith, describe, it, shouldBe)

doubleAreaSpec :: SpecWith ()
doubleAreaSpec =
  describe "Task2.doubleArea" $ do
    it "calculates double area for simple polygons" $ do
     doubleArea [Point 2 1, Point 4 5, Point 7 8] `shouldBe` 6
    it "calculates double area for another polygons" $ do
     doubleArea [Point 2 4, Point 3 (-8), Point 1 2] `shouldBe` 14
    it "calculates double area for another polygons" $ do
      doubleArea [Point 0 0, Point 0 1, Point 1 1, Point 1 0] `shouldBe` 2
    it "calculates double area for non-convex polygons" $ do
      doubleArea [Point 3 4, Point 5 11, Point 12 8, Point 9 5, Point 5 6] `shouldBe` 60

doubleAreaSlowSpec :: SpecWith ()
doubleAreaSlowSpec =
  describe "Task2.doubleAreaSlow" $ do
    it "calculates double area for simple polygons" $ do
     doubleAreaSlow [Point 2 1, Point 4 5, Point 7 8] `shouldBe` 6
    it "calculates double area for another polygons" $ do
      doubleAreaSlow [Point 2 4, Point 3 (-8), Point 1 2] `shouldBe` 14
    it "calculates double area for another polygons" $ do
      doubleAreaSlow [Point 0 0, Point 0 1, Point 1 1, Point 1 0] `shouldBe` 2
    it "calculates double area for non-convex polygons" $ do
      doubleAreaSlow [Point 3 4, Point 5 11, Point 12 8, Point 9 5, Point 5 6] `shouldBe` 60

perimeterSpec :: SpecWith ()
perimeterSpec =
  describe "Task2.perimeter" $ do
    it "calculates perimeter for polygons" $ do perimeter [Point 0 0, Point 0 1, Point 1 1, Point 1 0] `shouldBe` 4.0

perimeterSlowSpec :: SpecWith ()
perimeterSlowSpec =
  describe "Task2.perimeterSLow" $ do
    it "calculates perimeter for polygons" $ do
      perimeterSlow [Point 0 0, Point 0 1, Point 1 1, Point 1 0] `shouldBe` 4.0
