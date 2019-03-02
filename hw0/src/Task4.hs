module Task4
  ( factorial
  , iterateElement
  , fibonacci
  , mapFix
  ) where

import           Data.Function (fix)

factorial :: Integer -> Integer
factorial n = factorialAccum n 1
  where
    factorialAccum :: Integer -> Integer -> Integer
    factorialAccum = fix prefact
    prefact :: (Integer -> Integer -> Integer) -> Integer -> Integer -> Integer
    prefact f m acc
      | m <= 1 = acc
      | otherwise = f (m - 1) (acc * m)

iterateElement :: a -> [a]
iterateElement = fix preIterateElement
  where
    preIterateElement :: (a -> [a]) -> a -> [a]
    preIterateElement f x = x : f x

fibonacci :: Integer -> Integer
fibonacci number = fibonacciAcc number 1 1
  where
    fibonacciAcc :: Integer -> Integer -> Integer -> Integer
    fibonacciAcc = fix preFibonacci
    preFibonacci :: (Integer -> Integer -> Integer -> Integer) -> Integer -> Integer -> Integer -> Integer
    preFibonacci f n a b
      | n == 0 = a
      | otherwise = f (n - 1) b (a + b)

mapFix :: (a -> b) -> [a] -> [b]
mapFix g l = reverseList $ mapFixAccum g l []
  where
    mapFixAccum :: (a -> b) -> [a] -> [b] -> [b]
    mapFixAccum = fix preMapFix
    preMapFix :: ((a -> b) -> [a] -> [b] -> [b]) -> (a -> b) -> [a] -> [b] -> [b]
    preMapFix _ _ [] acc          = acc
    preMapFix f mapper (x:xs) acc = f mapper xs (mapper x : acc)
    reverseList :: [a] -> [a]
    reverseList list = reverseAccum list []
    reverseAccum :: [a] -> [a] -> [a]
    reverseAccum = fix preReverse
    preReverse :: ([a] -> [a] -> [a]) -> [a] -> [a] -> [a]
    preReverse _ [] acc     = acc
    preReverse f (x:xs) acc = f xs (x : acc)
