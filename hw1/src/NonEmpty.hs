{-# LANGUAGE InstanceSigs #-}

module NonEmpty
  ( NonEmpty(..)
  , reverseNonEmpty
  ) where

data NonEmpty a =
  a :| [a]
  deriving (Show)

instance Eq a => Eq (NonEmpty a) where
  (x :| xs) == (y :| ys) = (x == y) && (xs == ys)

instance Foldable NonEmpty where
  foldMap :: Monoid m => (a -> m) -> NonEmpty a -> m
  foldMap toMonoidMapper nonEmpty = foldMapAcc toMonoidMapper nonEmpty mempty
    where
      foldMapAcc :: Monoid m => (a -> m) -> NonEmpty a -> m -> m
      foldMapAcc mapper (x :| []) acc = mapper x <> acc
      foldMapAcc mapper (x :| (y:ys)) acc = foldMapAcc mapper (y :| ys) (mapper x <> acc)

  foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
  foldr function rightOne (x :| []) = function x rightOne
  foldr function rightOne (x :| (y:ys)) = foldrAcc function rightOne (y :| ys) (function x)
    where
      foldrAcc :: (a -> b -> b) -> b -> NonEmpty a -> (b -> b) -> b
      foldrAcc f z (curElem :| []) accFunction = accFunction (f curElem z)
      foldrAcc f z (curElem :| (nextElem:otherElems)) accFunction =
        foldrAcc f z (nextElem :| otherElems) (accFunction . f curElem)

instance Functor NonEmpty where
  fmap :: (a -> b) -> NonEmpty a -> NonEmpty b
  fmap f (x :| xs) = f x :| fmap f xs

-- | Reverses NonEmpty, as a non-empty list
-- >>> reverseNonEmpty (1 :| [2, 3, 4]) == 4 :| [3, 2, 1]
-- True
-- >>> reverseNonEmpty (1 :| []) == 1 :| []
-- True
-- >>> reverseNonEmpty (1 :| [2, 3, 4]) == 1 :| [4, 3, 2]
-- False
reverseNonEmpty :: NonEmpty a -> NonEmpty a
reverseNonEmpty orig@(_ :| []) = orig
reverseNonEmpty (first :| (second:others)) = reverseNonEmptyAcc (second :| others) (first :| [])
  where
    reverseNonEmptyAcc :: NonEmpty a -> NonEmpty a -> NonEmpty a
    reverseNonEmptyAcc (x :| []) (y :| accTail) = x :| (y : accTail)
    reverseNonEmptyAcc (x :| (nextX:tailX)) (y :| accTail) = reverseNonEmptyAcc (nextX :| tailX) (x :| (y : accTail))

instance Semigroup (NonEmpty a) where
  (<>) (x :| xs) (y :| ys) = x :| (xs ++ [y] ++ ys)
