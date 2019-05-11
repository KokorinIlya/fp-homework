module Task1Bench
  ( evalBench
  ) where

import Task1 (multiplyV1, multiplyV2, multiplyV3, multiplyV4)

import Criterion.Main (bench, bgroup, defaultMain, nf)

matrixGenerate :: Int -> Int -> Int -> [[Int]]
matrixGenerate a b c = replicate a $ replicate b c

evalBench :: IO ()
evalBench =
  defaultMain
    [ bgroup "matrix mul" matrixMultV1
    , bgroup "matrix mul" matrixMultV2
    , bgroup "matrix mul" matrixMultV3
    , bgroup "matrix mul" matrixMultV4
    ]
  where
    matrixs = map (\x -> (x, matrixGenerate x x 5, matrixGenerate x x 5)) [100, 200, 300, 400, 500]
    matrixMultV1 = map (\(a, l, r) -> bench ("transpose, parallel " <> show a) $ nf (multiplyV1 l) r) matrixs
    matrixMultV2 = map (\(a, l, r) -> bench ("no-transpose, parallel " <> show a) $ nf (multiplyV2 l) r) matrixs
    matrixMultV3 = map (\(a, l, r) -> bench ("no-transpose, no-parallel " <> show a) $ nf (multiplyV3 l) r) matrixs
    matrixMultV4 =
      map (\(a, l, r) -> bench ("no-transpose, accurate parallel " <> show a) $ nf (multiplyV4 l) r) matrixs
