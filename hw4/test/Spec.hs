module Main
  ( main
  ) where

import Task2Spec (doubleAreaSlowSpec, doubleAreaSpec, perimeterSlowSpec, perimeterSpec)
import Task3Spec (gaussSpec)
import Task4Spec (hashTableSpec)
import Test.Hspec (hspec)

main :: IO ()
main =
  hspec $ do
    doubleAreaSpec
    doubleAreaSlowSpec
    perimeterSpec
    perimeterSlowSpec
    gaussSpec
    hashTableSpec
