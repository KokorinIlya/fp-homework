module Task1
  ( stringSum
  ) where

import Text.Read (readEither)

-- TODO : Make it work for num
stringSum :: (Num t, Read t) => String -> Maybe t
stringSum = fmap sum . traverse readMaybe . words
  where
    readMaybe :: (Num a, Read a) => String -> Maybe a
    readMaybe toRead =
      case readEither toRead of
        Right result -> Just result
        Left _       -> Nothing

