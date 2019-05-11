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
crossProduct Point {x = x1, y = y1} Point {x = x2, y = y2} = x1 * y2 + x2 * y1

doubleArea :: [Point] -> Int
doubleArea [] = 0
doubleArea pointsList@(firstPoint:_) =
  let pointsPairs = zip pointsList (tail pointsList)
   in abs $ foldl' (+) 0 (map (uncurry crossProduct) pointsPairs) + crossProduct (last pointsList) firstPoint

perimeter :: [Point] -> Double
perimeter [] = 0.0
perimeter pointsList@(firstPoint:_) =
  let pointsPairs = zip pointsList (tail pointsList)
   in foldl' (+) 0 (map (uncurry mapper) pointsPairs) + mapper (last pointsList) firstPoint
  where
    mapper :: Point -> Point -> Double
    mapper p1 p2 =
      let Point {x = curX, y = curY} = p1 `minus` p2
       in sqrt $ fromIntegral (curX * curX) + fromIntegral (curY * curY)
