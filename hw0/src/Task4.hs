module Task4
       (
         factorial
       , iterateElement
       , fibonacci
       , mapFix
       ) where

import Data.Function (fix)

factorial :: Integer -> Integer
factorial = fix prefact
  where
    prefact :: (Integer -> Integer) -> Integer -> Integer
    prefact = \f -> \n ->
      if (n <= 1)
      then 1
      else n * f (n - 1)

iterateElement :: a -> [a]
iterateElement = fix preIterateElement
  where
    preIterateElement :: (a -> [a]) -> a -> [a]
    preIterateElement f x = x:(f x)

fibonacci :: Integer -> Integer
fibonacci = fix preFibonacci
  where
    preFibonacci :: (Integer -> Integer) -> Integer -> Integer
    preFibonacci f n | n == 0 || n == 1 = 1
                     | otherwise = f (n - 1) + f (n - 2)

mapFix :: (a -> b) -> [a] -> [b]
mapFix = fix preMapFix
  where
    preMapFix :: ((a -> b) -> [a] -> [b]) -> (a -> b) -> [a] -> [b]
    preMapFix _ _ [] = []
    preMapFix f mapper (x:xs) = (mapper x):(f mapper xs)
