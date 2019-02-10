{-# LANGUAGE TypeOperators #-}

module Task1
       (
         distributivity
       , associator
       , eitherAssoc
       ) where

distributivity
   :: Either a (b, c)
   -> (Either a b, Either a c)
distributivity (Left aValue) = (Left aValue, Left aValue)
distributivity (Right (bValue, cValue)) = (Right bValue, Right cValue)

associator
    :: (a, (b, c))
    -> ((a, b), c)
associator (aValue, (bValue, cValue)) = ((aValue, bValue), cValue)

type (<->) a b = (a -> b, b -> a)

eitherAssoc
    :: Either a (Either b c)
    <-> Either (Either a b) c
eitherAssoc =
  (firstFunction, secondFunction)

  where
    firstFunction :: Either a (Either b c) -> Either (Either a b) c
    firstFunction (Left aValue) = Left (Left aValue)
    firstFunction (Right (Left bValue)) = Left (Right bValue)
    firstFunction (Right (Right cValue)) = Right cValue

    secondFunction :: Either (Either a b) c -> Either a (Either b c)
    secondFunction (Left (Left aValue)) = Left aValue
    secondFunction (Left (Right bValue)) = Right (Left bValue)
    secondFunction (Right cValue) = Right (Right cValue)
