{-# LANGUAGE BangPatterns #-}

module Task1
  ( multiplyV1
  , multiplyV2
  , multiplyV3
  , multiplyV4
  , multiply
  , multiplyNaive
  ) where

import Control.Monad (forM_, join)
import Control.Monad.ST (ST)
import Control.Parallel.Strategies (Eval, rpar, runEval)
import Data.STRef (STRef, modifySTRef', newSTRef, readSTRef, writeSTRef)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Task2 (parallelReduce)

import Control.Parallel.Strategies (parListChunk, parMap, rdeepseq)
import Data.Array.Unboxed (UArray, bounds, listArray, (!))
import qualified Data.List as DL

type Multiplier = UArray (Int, Int) Int -> UArray (Int, Int) Int -> [[Int]]

listToArrayMatrix :: [[Int]] -> UArray (Int, Int) Int
listToArrayMatrix mat = do
  let n = length mat
  let m = length $ head mat
  listArray ((0, 0), (n - 1, m - 1)) $ concat mat

mulArrayMatrixSeq :: Multiplier
mulArrayMatrixSeq ma mb = [[sum [(ma ! (i, t)) * (mb ! (t, j)) | t <- [0 .. m]] | j <- [0 .. k]] | i <- [0 .. n]]
  where
    n = fst $ snd $ bounds ma
    m = snd $ snd $ bounds ma
    k = snd $ snd $ bounds mb

mulArrayMatrixPar :: Multiplier
mulArrayMatrixPar ma mb = parMap (parListChunk 10000 rdeepseq) countRow [[(i, j) | j <- [0 .. k]] | i <- [0 .. n]]
  where
    n = fst $ snd $ bounds ma
    m = snd $ snd $ bounds ma
    k = snd $ snd $ bounds mb
    countProd !i !j !t = (ma ! (i, t)) * (mb ! (t, j))
    countCell (!i, !j) = DL.foldl' (+) 0 $ map (countProd i j) [0 .. m]
    countRow = map countCell

multiplyCore :: Multiplier -> [[Int]] -> [[Int]] -> Maybe [[Int]]
multiplyCore inner ma mb =
  if length (head ma) /= length mb
    then Nothing
    else Just $ inner (listToArrayMatrix ma) (listToArrayMatrix mb)

multiplyNaive :: [[Int]] -> [[Int]] -> Maybe [[Int]]
multiplyNaive = multiplyCore mulArrayMatrixSeq

multiply :: [[Int]] -> [[Int]] -> Maybe [[Int]]
multiply = multiplyCore mulArrayMatrixPar

{-
Вторая матрица не транспонируется
Паралеллизм: 1 спарк на несколько строк (4)
-}
multiplyV4 :: [[Int]] -> [[Int]] -> Maybe [[Int]]
multiplyV4 [] _ = Nothing
multiplyV4 _ [] = Nothing
multiplyV4 first@(firstX:_) second@(secondX:_) =
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
    multiplyImpl n1 m1 m2 v1 v2 = parallelReduce join 1 (map (multyplyRow m1 m2 v1 v2)) [0 .. n1 - 1]
    multyplyRow :: Int -> Int -> V.Vector Int -> V.Vector Int -> Int -> [Int]
    multyplyRow m1 m2 v1 v2 !i =
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
multiplyV1 :: [[Int]] -> [[Int]] -> Maybe [[Int]]
multiplyV1 [] _ = Nothing
multiplyV1 _ [] = Nothing
multiplyV1 first@(firstX:_) second@(secondX:_) =
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
        mVect <- MV.new (V.length vect) :: ST s (MV.MVector s Int)
        forM_ [0 .. n - 1] $ \i ->
          forM_ [0 .. m - 1] $ \j ->
            let !firstInd = i * m + j
                !secondInd = j * n + i
             in MV.write mVect secondInd $! vect V.! firstInd
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
            let !firstNum = v1 V.! firstInd
            let !secondNum = v2Transposed V.! secondInd
            modifySTRef' crossProduct (+ firstNum * secondNum)
          !finalCrossProduct <- readSTRef crossProduct
          MV.write mVect j finalCrossProduct
        return mVect
