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
  , doubleAreaSlow
  , perimeterSlow
  ) where

import Data.List (foldl')

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
crossProduct Point {x = x1, y = y1} Point {x = x2, y = y2} = x1 * y2 - x2 * y1

sum' :: Num a => [a] -> a
sum' = foldl' (+) 0

doubleAreaSlow :: [Point] -> Int
doubleAreaSlow [] = 0
doubleAreaSlow pointsList@(firstPoint:_) =
  let pointsPairs = zip pointsList (tail pointsList)
   in abs $ sum' (map (uncurry crossProduct) pointsPairs) + crossProduct (last pointsList) firstPoint

perimeterSlow :: [Point] -> Double
perimeterSlow [] = 0.0
perimeterSlow pointsList@(firstPoint:_) =
  let pointsPairs = zip pointsList (tail pointsList)
   in sum' (map (uncurry mapper) pointsPairs) + mapper (last pointsList) firstPoint
  where
    mapper :: Point -> Point -> Double
    mapper p1 p2 =
      let Point {x = curX, y = curY} = p1 `minus` p2
       in sqrt $ fromIntegral (curX * curX) + fromIntegral (curY * curY)

doubleArea :: [Point] -> Int
doubleArea [] = 0
doubleArea (p:ps) = abs $ doubleAreaImpl p (p : ps) 0
  where
    doubleAreaImpl :: Point -> [Point] -> Int -> Int
    doubleAreaImpl _ [] _ = 0
    doubleAreaImpl firstPoint [lastPoint] !acc = acc + crossProduct lastPoint firstPoint
    doubleAreaImpl firstPoint (curPoint:nextPoint:otherPoints) !acc =
      let !curRes = crossProduct curPoint nextPoint
          !curAcc = acc + curRes
       in doubleAreaImpl firstPoint (nextPoint : otherPoints) curAcc

perimeter :: [Point] -> Double
perimeter [] = 0.0
perimeter (p:ps) = perimeterImpl p (p : ps) 0
  where
    dist :: Point -> Point -> Double
    dist p1 p2 =
      let Point {x = curX, y = curY} = p1 `minus` p2
       in sqrt $ fromIntegral (curX * curX) + fromIntegral (curY * curY)
    perimeterImpl :: Point -> [Point] -> Double -> Double
    perimeterImpl _ [] _ = 0
    perimeterImpl firstPoint [lastPoint] !acc = acc + dist lastPoint firstPoint
    perimeterImpl firstPoint (curPoint:nextPoint:otherPoints) !acc =
      let !curRes = dist curPoint nextPoint
          !curAcc = curRes + acc
       in perimeterImpl firstPoint (nextPoint : otherPoints) curAcc
