module Lib
       ( plusTwo,
       checkPM
       ) where

import Data.Function (fix)

plusTwo :: [Int] -> [Int]
plusTwo = map (+2)

checkPM :: [Int] -> Int
checkPM [] = 0
checkPM (x1:x2:xs)
        | x1 < 0 = -1
        | x1 > 0 = 1
checkPM (x:xs) = 2

someList = let x = 1 : y
               y = 2 : x
               in x

x :: [Int]
x = 1 : y

y :: [Int]
y = 2 : x

prefact :: (Int -> Int) -> Int -> Int
prefact f n = if n == 0 then 1 else n * (f (n - 1))

fact :: Int -> Int
fact = fix prefact
