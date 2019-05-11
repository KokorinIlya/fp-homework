{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Task2
  ( Point(..)
  , plus
  , minus
  , scalarProduct
  , crossProduct
  , doubleArea
  , perimeter
  , parallelReduce
  ) where

import Control.Parallel.Strategies (Eval, rpar, runEval)
import Data.List (foldl')
import qualified Data.Vector as V

data Point = Point
  { x :: !Int
  , y :: !Int
  }

plus :: Point -> Point -> Point
plus Point {x = x1, y = y1} Point {x = x2, y = y2} = Point {x = x1 + x2, y = y1 + y2}

minus :: Point -> Point -> Point
minus Point {x = x1, y = y1} Point {x = x2, y = y2} = Point {x = x1 - x2, y = y1 - y2}

scalarProduct :: Point -> Point -> Int
scalarProduct Point {x = x1, y = y1} Point {x = x2, y = y2} = x1 * x2 + y1 * y2

crossProduct :: Point -> Point -> Int
crossProduct Point {x = x1, y = y1} Point {x = x2, y = y2} = x1 * y2 + x2 * y1

workPartition :: forall a b. Int -> ([a] -> b) -> [a] -> [b]
workPartition !tasksPerSpark mapper = runEval . workPartitionImpl
  where
    workPartitionImpl :: [a] -> Eval [b]
    workPartitionImpl tasks = do
      let (tasksForCurSpark, otherTasks) = splitAt tasksPerSpark tasks
      case otherTasks of
        []    -> return [mapper tasksForCurSpark]
        (_:_) -> (:) <$> rpar (mapper tasksForCurSpark) <*> workPartitionImpl otherTasks

sum' :: Num a => [a] -> a
sum' = foldl' (+) 0

parallelReduce :: ([b] -> c) -> Int -> ([a] -> b) -> [a] -> c
parallelReduce g n f = g . workPartition n f

doubleArea :: [Point] -> Int
doubleArea pointsList =
  let pointsVector = V.fromList pointsList
      n = V.length pointsVector
      tasksPerSpark = round $ sqrt (fromIntegral n :: Double)
   in parallelReduce (abs . sum') tasksPerSpark (sum' . fmap (calculateIteration n pointsVector)) [0 .. n - 1]
  where
    calculateIteration :: Int -> V.Vector Point -> Int -> Int
    calculateIteration !n v !i =
      let (!prevInd, !nextInd) =
            if | i == 0 -> (n - 1, 1)
               | i == n - 1 -> (n - 2, 0)
               | otherwise -> (i - 1, i + 1)
          !xCur = x $ v V.! i
          !yPrev = y $ v V.! prevInd
          !yNext = y $ v V.! nextInd
       in xCur * (yNext - yPrev)

perimeter :: [Point] -> Double
perimeter pointsList =
  let pointsVector = V.fromList pointsList
      n = V.length pointsVector
      tasksPerSpark = round $ sqrt (fromIntegral n :: Double)
   in parallelReduce sum' tasksPerSpark (sum' . fmap (calculateIteration n pointsVector)) [0 .. n - 1]
  where
    calculateIteration :: Int -> V.Vector Point -> Int -> Double
    calculateIteration !n v !i =
      let (!curInd, !nextInd) =
            if i == n - 1
              then (n - 1, 0)
              else (i, i + 1)
          curPoint = v V.! curInd
          nextPoint = v V.! nextInd
          !curX = x curPoint
          !curY = y curPoint
          !nextX = x nextPoint
          !nextY = y nextPoint
          !dx = fromIntegral $ curX - nextX :: Double
          !dy = fromIntegral $ curY - nextY :: Double
       in sqrt $ dx * dx + dy * dy
