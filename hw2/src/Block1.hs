module Block1
  ( NonEmpty(..)
  , Tree(..)
  , stringSum
  ) where

import Text.Read (readMaybe)

stringSum :: (Num t, Read t) => String -> Maybe t
stringSum = fmap sum . traverse readMaybe . words

data Tree a
  = Branch (Tree a) (Tree a)
  | Leaf a

instance Show a => Show (Tree a) where
  show tree = showLevel tree 0
    where
      showLevel :: Show a => Tree a -> Int -> String
      showLevel (Leaf value) level = replicate level '-' ++ (' ' : show value)
      showLevel (Branch left right) level =
        let begining = replicate level '-'
            shownLeft = showLevel left (level + 1)
            shownRight = showLevel right (level + 1)
         in begining ++ "\n" ++ shownLeft ++ "\n" ++ shownRight

instance Functor Tree where
  fmap mapper (Leaf value)        = Leaf $ mapper value
  fmap mapper (Branch left right) = Branch (fmap mapper left) (fmap mapper right)

instance Foldable Tree where
  foldr f z (Leaf a)            = f a z
  foldr f z (Branch left right) = foldr f (foldr f z right) left

instance Applicative Tree where
  pure = Leaf

  --(<*>) = ap
  Leaf f <*> valueTree            = f <$> valueTree
  Branch left right <*> valueTree = Branch (left <*> valueTree) (right <*> valueTree)

instance Monad Tree where
  Leaf value >>= f        = f value
  Branch left right >>= f = Branch (left >>= f) (right >>= f)

instance Traversable Tree where
  traverse f (Leaf value)        = Leaf <$> f value
  traverse f (Branch left right) = Branch <$> traverse f left <*> traverse f right

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
        listTail = xs >>= (toList . f)
     in y :| (ys ++ listTail)
    where
      toList :: NonEmpty a -> [a]
      toList (first :| others) = first : others

instance Foldable NonEmpty where
  foldr function rightOne (x :| []) = function x rightOne
  foldr function rightOne (x :| (y:ys)) = foldrAcc function rightOne (y :| ys) (function x)
    where
      foldrAcc :: (a -> b -> b) -> b -> NonEmpty a -> (b -> b) -> b
      foldrAcc f z (curElem :| []) accFunction = accFunction (f curElem z)
      foldrAcc f z (curElem :| (nextElem:otherElems)) accFunction =
        foldrAcc f z (nextElem :| otherElems) (accFunction . f curElem)

instance Traversable NonEmpty where
  traverse f (x :| xs) = (:|) <$> f x <*> traverse f xs
