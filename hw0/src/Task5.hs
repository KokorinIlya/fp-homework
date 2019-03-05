module Task5
  ( Nat
  , churchFromInteger
  , churchMult
  , churchPlus
  , churchToInt
  , churchToNum
  , succChurch
  , zero
  ) where

type Nat a = (a -> a) -> a -> a

zero :: Nat a
zero _ x = x

succChurch :: Nat a -> Nat a
succChurch n f z = n f (f z)

churchToInt :: Nat Integer -> Integer
churchToInt n = n (+ 1) 0

churchToNum :: Num t => Nat t -> t
churchToNum n = n (+ 1) 0

churchPlus :: Nat a -> Nat a -> Nat a
churchPlus n m f z = n f (m f z)

churchMult :: Nat a -> Nat a -> Nat a
churchMult n m f = n (m f)

churchFromInteger :: Integer -> Nat a
churchFromInteger n = churchFromIntegerAcc n zero
  where
    churchFromIntegerAcc :: Integer -> Nat a -> Nat a
    churchFromIntegerAcc m acc
      | m > 0     = churchFromIntegerAcc (m - 1) (\f -> f . acc f)
      | otherwise = acc
