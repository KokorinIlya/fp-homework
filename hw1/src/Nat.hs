module Nat
  ( Nat
  , isEven
  , isOdd
  ) where

import Data.Ratio ((%))

data Nat
  = Z
  | S Nat

instance Eq Nat where
  (==) Z Z         = True
  (==) Z (S _)     = False
  (==) (S _) Z     = False
  (==) (S a) (S b) = a == b

instance Ord Nat where
  compare Z Z         = EQ
  compare (S _) Z     = GT
  compare Z (S _)     = LT
  compare (S a) (S b) = compare a b

-- | Converts some number to Peano number
-- >>> let numbers = [1,17,25,16,2,3,8]
-- >>> let numbersPairs = [(x, y) | x <- numbers, y <- numbers]
-- >>> filter (\(a,b) -> (fromNumber a) + (fromNumber b) /= fromNumber (a + b)) numbersPairs
-- []
-- >>> filter (\(a,b) -> (fromNumber a) * (fromNumber b) /= fromNumber (a * b)) numbersPairs
-- []
-- >>> filter (\(a,b) -> max 0 ((fromNumber a) - (fromNumber b)) /= fromNumber (a - b)) numbersPairs
-- []
-- >>> let getQuotRemPeano (a,b) = (fromNumber a, fromNumber b)
-- >>> filter (\(a,b) -> quotRem (fromNumber a) (fromNumber b) /= getQuotRemPeano (quotRem a b)) numbersPairs
-- []
fromNumber :: (Ord a, Num a) => a -> Nat
fromNumber x
  | x > 0 = S $ fromNumber $ x - 1
  | otherwise = Z

toNumber :: Num a => Nat -> a
toNumber n = toNumberAcc n 0
  where
    toNumberAcc :: Num a => Nat -> a -> a
    toNumberAcc Z acc     = acc
    toNumberAcc (S k) acc = toNumberAcc k (acc + 1)

instance Num Nat where
  (+) Z k     = k
  (+) (S n) k = S (n + k)
  abs = id
  signum Z     = Z
  signum (S _) = S Z
  fromInteger = fromNumber
  (*) Z _     = Z
  (*) (S k) n = k * n + n
  (-) Z _         = Z
  (-) k Z         = k
  (-) (S k) (S n) = k - n

instance Show Nat where
  show = show . (toNumber :: Nat -> Integer)

-- | Tests, if number is even
-- >>> isEven $ S $ S $ S $ S $ Z
-- True
-- >>> isEven $ S $ S $ S $ Z
-- False
isEven :: Nat -> Bool
isEven Z     = True
isEven (S k) = not $ isEven k

-- | Tests, if number is odd
-- >>> isOdd $ S $ S $ S $ S $ Z
-- False
-- >>> isOdd $ S $ S $ S $ Z
-- True
isOdd :: Nat -> Bool
isOdd = not . isEven

instance Enum Nat where
  toEnum = fromNumber
  fromEnum = toNumber

instance Real Nat where
  toRational n = toNumber n % 1

instance Integral Nat where
  toInteger = toNumber
  quotRem _ Z = error "Division by zero"
  quotRem n k
    | n < k = (0, n)
    | otherwise =
      let (divisionResult, reminder) = quotRem (n - k) k
       in (divisionResult + 1, reminder)
