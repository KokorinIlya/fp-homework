module Task1Bench
  ( matrixBench
  ) where

import Task1 (multiply, multiplyV2, multiplyV3)

import Criterion.Main (bench, bgroup, defaultMain, nf)

matrixGenerate :: Int -> Int -> Int -> [[Int]]
matrixGenerate a b c = replicate a $ replicate b c

matrixBench :: IO ()
matrixBench =
  defaultMain
    [ bgroup "matrix mul" matrixMult
    , bgroup "matrix mul" matrixMultV2
    , bgroup "matrix mul" matrixMultV3
    ]
  where
    matrixs = map (\x -> (x, matrixGenerate x x 5, matrixGenerate x x 5)) [100, 300, 500]
    matrixMult = map (\(a, l, r) -> bench ("transpose, parallel " <> show a) $ nf (multiply l) r) matrixs
    matrixMultV2 = map (\(a, l, r) -> bench ("no-transpose, parallel " <> show a) $ nf (multiplyV2 l) r) matrixs
    matrixMultV3 = map (\(a, l, r) -> bench ("no-transpose, no-parallel " <> show a) $ nf (multiplyV3 l) r) matrixs
