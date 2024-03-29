{-# LANGUAGE BangPatterns #-}

module Task1
  ( multiply
  , multiplyV2
  , multiplyV3
  ) where

import Control.Monad (forM_, join)
import Control.Monad.ST (ST)
import Control.Parallel.Strategies (Eval, rpar, runEval)
import Data.STRef (STRef, modifySTRef', newSTRef, readSTRef, writeSTRef)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

{-
Вторая матрица не транспонируется
Паралеллизм: нет
-}
multiplyV3 :: [[Int]] -> [[Int]] -> Maybe [[Int]]
multiplyV3 [] _ = Nothing
multiplyV3 _ [] = Nothing
multiplyV3 first@(firstX:_) second@(secondX:_) =
  let !n1 = length first
      !n2 = length second
      !m1 = length firstX
      !m2 = length secondX
   in if n2 == m1
        then let v1 = V.fromList $ join first
                 v2 = V.fromList $ join second
              in Just $ multyplyParallel 0 n1 m1 m2 v1 v2
        else Nothing
  where
    multyplyParallel :: Int -> Int -> Int -> Int -> V.Vector Int -> V.Vector Int -> [[Int]]
    multyplyParallel !i n1 m1 m2 v1 v2
      | i == n1 = []
      | otherwise = multyplyRow i m1 m2 v1 v2 : multyplyParallel (i + 1) n1 m1 m2 v1 v2
    multyplyRow :: Int -> Int -> Int -> V.Vector Int -> V.Vector Int -> [Int]
    multyplyRow !i m1 m2 v1 v2 =
      V.toList $
      V.create $ do
        mVect <- MV.new m2 :: ST s (MV.MVector s Int)
        crossProduct <- newSTRef 0 :: ST s (STRef s Int)
        forM_ [0 .. m2 - 1] $ \j -> do
          writeSTRef crossProduct 0
          forM_ [0 .. m1 - 1] $ \k -> do
            let !firstInd = i * m1 + k
            let !secondInd = k * m2 + j
            let !firstNum = v1 V.! firstInd
            let !secondNum = v2 V.! secondInd
            modifySTRef' crossProduct (+ firstNum * secondNum)
          !finalCrossProduct <- readSTRef crossProduct
          MV.write mVect j finalCrossProduct
        return mVect

{-
Вторая матрица не транспонируется
Паралеллизм: 1 спарк на строку
-}
multiplyV2 :: [[Int]] -> [[Int]] -> Maybe [[Int]]
multiplyV2 [] _ = Nothing
multiplyV2 _ [] = Nothing
multiplyV2 first@(firstX:_) second@(secondX:_) =
  let !n1 = length first
      !n2 = length second
      !m1 = length firstX
      !m2 = length secondX
   in if n2 == m1
        then let v1 = V.fromList $ join first
                 v2 = V.fromList $ join second
              in Just $ multiplyImpl n1 m1 m2 v1 v2
        else Nothing
  where
    multiplyImpl :: Int -> Int -> Int -> V.Vector Int -> V.Vector Int -> [[Int]]
    multiplyImpl n1 m1 m2 v1 v2 = runEval $ multyplyParallel 0 n1 m1 m2 v1 v2
    multyplyParallel :: Int -> Int -> Int -> Int -> V.Vector Int -> V.Vector Int -> Eval [[Int]]
    multyplyParallel !i n1 m1 m2 v1 v2
      | i == n1 = return []
      | otherwise = (:) <$> rpar (multyplyRow i m1 m2 v1 v2) <*> multyplyParallel (i + 1) n1 m1 m2 v1 v2
    multyplyRow :: Int -> Int -> Int -> V.Vector Int -> V.Vector Int -> [Int]
    multyplyRow !i m1 m2 v1 v2 =
      V.toList $
      V.create $ do
        mVect <- MV.new m2 :: ST s (MV.MVector s Int)
        crossProduct <- newSTRef 0 :: ST s (STRef s Int)
        forM_ [0 .. m2 - 1] $ \j -> do
          writeSTRef crossProduct 0
          forM_ [0 .. m1 - 1] $ \k -> do
            let !firstInd = i * m1 + k
            let !secondInd = k * m2 + j
            let !firstNum = v1 V.! firstInd
            let !secondNum = v2 V.! secondInd
            modifySTRef' crossProduct (+ firstNum * secondNum)
          !finalCrossProduct <- readSTRef crossProduct
          MV.write mVect j finalCrossProduct
        return mVect

{-
Вторая матрица транспонируется (меньше кеш-промахов во время проходов по j-ому столбцу)
Паралеллизм: 1 спарк на строку
-}
multiply :: [[Int]] -> [[Int]] -> Maybe [[Int]]
multiply [] _ = Nothing
multiply _ [] = Nothing
multiply first@(firstX:_) second@(secondX:_) =
  let !n1 = length first
      !n2 = length second
      !m1 = length firstX
      !m2 = length secondX
   in if n2 == m1
        then let v1 = V.fromList $ join first
                 v2 = V.fromList $ join second
                 v2Transposed = transpose n2 m2 v2
              in Just $ multiplyImpl n1 m1 n2 m2 v1 v2Transposed
        else Nothing
  where
    transpose :: Int -> Int -> V.Vector Int -> V.Vector Int
    transpose n m vect =
      V.create $ do
        mVect <- MV.unsafeNew (V.length vect) :: ST s (MV.MVector s Int)
        forM_ [0 .. n - 1] $ \i ->
          forM_ [0 .. m - 1] $ \j ->
            let !firstInd = i * m + j
                !secondInd = j * n + i
             in MV.unsafeWrite mVect secondInd $! V.unsafeIndex vect firstInd
        return mVect
    multiplyImpl :: Int -> Int -> Int -> Int -> V.Vector Int -> V.Vector Int -> [[Int]]
    multiplyImpl n1 m1 n2 m2 v1 v2Transposed = runEval $ multyplyParallel 0 n1 m1 n2 m2 v1 v2Transposed
    multyplyParallel :: Int -> Int -> Int -> Int -> Int -> V.Vector Int -> V.Vector Int -> Eval [[Int]]
    multyplyParallel !i n1 m1 n2 m2 v1 v2Transposed
      | i == n1 = return []
      | otherwise =
        (:) <$> rpar (multyplyRow i m1 n2 m2 v1 v2Transposed) <*> multyplyParallel (i + 1) n1 m1 n2 m2 v1 v2Transposed
    multyplyRow :: Int -> Int -> Int -> Int -> V.Vector Int -> V.Vector Int -> [Int]
    multyplyRow !i m1 n2 m2 v1 v2Transposed =
      V.toList $
      V.create $ do
        mVect <- MV.new m2 :: ST s (MV.MVector s Int)
        crossProduct <- newSTRef 0 :: ST s (STRef s Int)
        forM_ [0 .. m2 - 1] $ \j -> do
          writeSTRef crossProduct 0
          forM_ [0 .. m1 - 1] $ \k -> do
            let !firstInd = i * m1 + k
            let !secondInd = j * n2 + k
            let !firstNum = V.unsafeIndex v1 firstInd
            let !secondNum = V.unsafeIndex v2Transposed secondInd
            modifySTRef' crossProduct (+ firstNum * secondNum)
          !finalCrossProduct <- readSTRef crossProduct
          MV.unsafeWrite mVect j finalCrossProduct
        return mVect
