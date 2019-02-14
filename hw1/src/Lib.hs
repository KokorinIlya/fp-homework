module Lib
       ( plusTwo,
       checkPM
       ) where


import Data.Foldable
import Data.Function (fix)

plusTwo :: [Int] -> [Int]
plusTwo = map (+2)

checkPM :: [Int] -> Int
checkPM [] = 0
checkPM (x1:x2:xs)
        | x1 < 0 = -1
        | x1 > 0 = 1
checkPM (x:xs) = 2

mapX :: (a -> b) -> [a] -> [b]
mapX _ []     = []
mapX f (x:xs) = f x : map f xs

myMap :: (a -> b) -> [a] -> [b]
myMap f l = mapAcc f l []
  where
    mapAcc :: (a -> b) -> [a] -> [b] -> [b]
    mapAcc _ [] acc = acc
    mapAcc mapper (x:xs) acc = mapAcc mapper xs ((mapper x):acc)
