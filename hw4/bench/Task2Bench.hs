module Task2Bench
  ( doubleAreaBench
  , perimeterBench
  ) where

import Task2 (Point (..), doubleArea, doubleAreaSlow, perimeter, perimeterSlow)

import Criterion.Main (bench, bgroup, defaultMain, nf)

pointsListGenerate :: Int -> [Point]
pointsListGenerate n = replicate n $ Point 1 1

tenPow :: Int -> Int
tenPow = (10 ^)

pointLists :: [(Int, [Point])]
pointLists = map (\n -> (n, pointsListGenerate n)) [tenPow 3, tenPow 5, tenPow 7]

doubleAreaBench :: IO ()
doubleAreaBench = defaultMain [bgroup "double area" doubleAreaEval, bgroup "double area" doubleAreaSlowEval]
  where
    doubleAreaEval = map (\(n, ps) -> bench ("fast method " <> show n) $ nf doubleArea ps) pointLists
    doubleAreaSlowEval = map (\(n, ps) -> bench ("slow method " <> show n) $ nf doubleAreaSlow ps) pointLists

perimeterBench :: IO ()
perimeterBench = defaultMain [bgroup "perimeter" perimeterEval, bgroup "perimeter" perimeterSlowEval]
  where
    perimeterEval = map (\(n, ps) -> bench ("fast method " <> show n) $ nf perimeter ps) pointLists
    perimeterSlowEval = map (\(n, ps) -> bench ("slow method " <> show n) $ nf perimeterSlow ps) pointLists
