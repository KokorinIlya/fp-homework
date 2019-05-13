module Main where

import Task1Bench (matrixBench)
import Task2Bench (doubleAreaBench, perimeterBench)

main :: IO ()
main = do
  matrixBench
  doubleAreaBench
  perimeterBench
