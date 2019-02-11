module Task5
       (
         zero
       , Nat
       , succChurch
       , churchToInt
       , churchToNum
       , churchPlus
       , churchMult
       , churchFromNum
       ) where

type Nat a = (a -> a) -> a -> a

zero :: Nat a
zero _ x = x

succChurch :: Nat a -> Nat a
succChurch n f z = n f (f z)

churchToInt :: Nat Integer -> Integer
churchToInt n = n (+1) 0

churchToNum :: Num t => Nat t -> t
churchToNum n = n (+1) 0

churchPlus :: Nat a -> Nat a -> Nat a
churchPlus n m f z = n f (m f z)

churchMult :: Nat a -> Nat a -> Nat a
churchMult n m f z = n (m f) z

churchFromNum :: (Num t, Eq t) => t -> Nat a
churchFromNum n | n == 0    = zero
                | otherwise = \f -> \z -> f (churchFromNum (n - 1) f z)
