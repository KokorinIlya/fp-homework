module Trees
       (
         Tree
       , isEmpty
       , size
       , contains
       , insert
       ) where

import Data.List.NonEmpty( NonEmpty(..) )

data Tree a = Empty | Node (Tree a) (NonEmpty a) (Tree a)

isEmpty :: Tree a -> Bool
isEmpty Empty = True
isEmpty _     = False

size :: Tree a -> Int
size Empty                   = 0
size (Node left elems right) = (size left) + (length elems) + (size right)

contains :: Ord a => Tree a -> a -> Bool
contains Empty _                               = False
contains (Node left (x :| _) right) elemToFind = case (compare elemToFind x) of
  LT -> contains left elemToFind
  EQ -> True
  GT -> contains right elemToFind

insert :: Ord a => Tree a -> a -> Tree a
insert Empty elemToInsert
  = Node Empty (elemToInsert :| []) Empty
insert (Node left l@(x :| xs) right) elemToInsert
  = case (compare elemToInsert x) of
    LT ->
      let newLeft = insert left elemToInsert in
      Node newLeft l right

    EQ -> Node left (elemToInsert :| (x:xs)) right

    GT ->
      let newRight = insert right elemToInsert in
      Node left l newRight
