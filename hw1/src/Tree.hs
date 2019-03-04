{-# LANGUAGE InstanceSigs #-}

module Tree
  ( Tree
  , contains
  , delete
  , fromList
  , insert
  , isEmpty
  , size
  , toList
  ) where

import NonEmpty (NonEmpty (..))

data Tree a
  = Empty
  | Node (Tree a)
         (NonEmpty a)
         (Tree a)

isEmpty :: Tree a -> Bool
isEmpty Empty = True
isEmpty _     = False

size :: Tree a -> Int
size Empty                   = 0
size (Node left elems right) = size left + length elems + size right

contains :: Ord a => Tree a -> a -> Bool
contains Empty _ = False
contains (Node left (x :| _) right) elemToFind =
  case compare elemToFind x of
    LT -> contains left elemToFind
    EQ -> True
    GT -> contains right elemToFind

insert :: Ord a => Tree a -> a -> Tree a
insert Empty elemToInsert = Node Empty (elemToInsert :| []) Empty
insert (Node left l@(x :| xs) right) elemToInsert =
  case compare elemToInsert x of
    LT ->
      let newLeft = insert left elemToInsert
       in Node newLeft l right
    EQ -> Node left (elemToInsert :| (x : xs)) right
    GT ->
      let newRight = insert right elemToInsert
       in Node left l newRight

fromList :: Ord a => [a] -> Tree a
fromList = foldl insert Empty

toList :: Tree a -> [a]
toList = foldr addElem []
  where
    addElem :: a -> [a] -> [a]
    addElem = (:)
instance Show a => Show (Tree a) where
  show tree = showLevel tree 0
    where
      showLevel :: Show a => Tree a -> Int -> String
      showLevel Empty level = replicate level '-'
      showLevel (Node left (x :| xs) right) level =
        let begining = replicate level '-'
            shownList = show $ x : xs
            shownLeft = showLevel left (level + 1)
            shownRight = showLevel right (level + 1)
         in begining ++ shownList ++ "\n" ++ shownLeft ++ "\n" ++ shownRight

delete :: Ord a => Tree a -> a -> Tree a
delete Empty _ = Empty
delete (Node left l@(x :| xs) right) elemToDelete =
  case compare elemToDelete x of
    LT ->
      let newLeft = delete left elemToDelete
       in Node newLeft l right
    EQ ->
      case xs of
        [] ->
          case left of
            Empty -> right
            Node leftLeft leftList leftRight ->
              case right of
                Empty -> left
                Node {} ->
                  let (leftMax, newLeft) = extractMax leftLeft leftList leftRight
                   in Node newLeft leftMax right
        xHead:xTail -> Node left (xHead :| xTail) right
    GT ->
      let newRight = delete right elemToDelete
       in Node left l newRight
  where
    extractMax :: Tree a -> NonEmpty a -> Tree a -> (NonEmpty a, Tree a)
    extractMax leftSubTree list Empty = (list, leftSubTree)
    extractMax leftSubTree list (Node rightLeft rightList rightRight) =
      let (extractedMax, newRight) = extractMax rightLeft rightList rightRight
       in (extractedMax, Node leftSubTree list newRight)

instance Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap _ Empty = mempty
  foldMap f (Node left elems right) =
    let leftFolded = foldMap f left
        rightFolded = foldMap f right
        elemsFolded = foldMap f elems
     in leftFolded <> elemsFolded <> rightFolded
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ z Empty = z
  foldr f z (Node left elems right) =
    let rightFolded = foldr f z right
        elemsFolded = foldr f rightFolded elems
     in foldr f elemsFolded left
