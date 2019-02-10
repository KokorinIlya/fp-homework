module Task4
       (
         factorial
       --, smartReplicate
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
