module Main
  ( main
  ) where

import Task1Spec (stringSumSpec)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  Task1Spec.stringSumSpec

