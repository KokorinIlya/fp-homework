module Utils
  ( mapFirst
  ) where

mapFirst :: (a -> b) -> (a, c) -> (b, c)
mapFirst f (a, c) = (f a, c)
