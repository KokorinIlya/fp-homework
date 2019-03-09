module Block1
  ( Tree(..)
  , stringSum
  , NonEmpty(..)
  ) where

import Text.Read (readEither)
import Control.Applicative (liftA2)

stringSum :: (Num t, Read t) => String -> Maybe t
stringSum = fmap sum . traverse readMaybe . words
  where
    readMaybe :: (Num a, Read a) => String -> Maybe a
    readMaybe toRead =
      case readEither toRead of
        Right result -> Just result
        Left _       -> Nothing

data Tree a
  = Branch (Tree a)
           (Tree a)
  | Leaf a

instance Functor Tree where
  fmap mapper (Leaf value)        = Leaf $ mapper value
  fmap mapper (Branch left right) = Branch (fmap mapper left) (fmap mapper right)

-- TODO : Applicative Tree
instance Foldable Tree where
  foldr f z (Leaf a)            = f a z
  foldr f z (Branch left right) = foldr f (foldr f z right) left

-- TODO : Traversable Tree
data NonEmpty a =
  a :| [a]
  deriving (Show)

instance Functor NonEmpty where
  fmap f (x :| xs) = f x :| fmap f xs

instance Applicative NonEmpty where
  pure x = x :| []
  (f :| fs) <*> (x :| xs) = f x :| (fmap f xs ++ (fs <*> (x : xs)))

instance Monad NonEmpty where
  (x :| xs) >>= f =
    let (y :| ys) = f x
        toList (first :| others) = first : others
        listTail = xs >>= (toList . f)
     in y :| (ys ++ listTail)

instance Foldable NonEmpty where
  foldr function rightOne (x :| [])     = function x rightOne
  foldr function rightOne (x :| (y:ys)) = foldrAcc function rightOne (y :| ys) (function x)
    where
      foldrAcc :: (a -> b -> b) -> b -> NonEmpty a -> (b -> b) -> b
      foldrAcc f z (curElem :| []) accFunction = accFunction (f curElem z)
      foldrAcc f z (curElem :| (nextElem:otherElems)) accFunction =
        foldrAcc f z (nextElem :| otherElems) (accFunction . f curElem)

instance Traversable NonEmpty where
  traverse f (x :| xs) = liftA2 (:|) (f x) (traverse f xs)
