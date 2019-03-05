{-# LANGUAGE InstanceSigs #-}

module AATree
  ( AATree(..)
  , DeleteStrategy(..)
  , contains
  , deleteAllFromVertex
  , find
  , fromList
  , insert
  , isEmpty
  , size
  , toList
  ) where

import NonEmpty (NonEmpty (..))

data AATree a
  = AAEmpty
  | AANode Int
           (AATree a)
           (NonEmpty a)
           (AATree a)

skew :: AATree a -> AATree a
skew AAEmpty = AAEmpty
skew orig@(AANode _ AAEmpty _ _) = orig
skew orig@(AANode height (AANode leftHeight leftLeft leftElems leftRight) elems right)
  | height == leftHeight =
    let newRight = AANode height leftRight elems right
     in AANode leftHeight leftLeft leftElems newRight
  | otherwise = orig

split :: AATree a -> AATree a
split AAEmpty = AAEmpty
split orig@(AANode _ _ _ AAEmpty) = orig
split orig@(AANode _ _ _ (AANode _ _ _ AAEmpty)) = orig
split orig@(AANode height left elems (AANode rightHeight rightLeft rightElems rightRight@(AANode rightRightHeight _ _ _)))
  | height == rightRightHeight =
    let newLeft = AANode height left elems rightLeft
     in AANode (rightHeight + 1) newLeft rightElems rightRight
  | otherwise = orig

-- | Returns tree with specified element inserted
-- >>> toList $ insert AAEmpty 3
-- [3]
-- >>> toList $ insert (fromList [1,7,3]) 3
-- [1,3,3,7]
insert :: Ord a => AATree a -> a -> AATree a
insert AAEmpty elemToInsert = AANode 1 AAEmpty (elemToInsert :| []) AAEmpty
insert (AANode height left elems@(curElem :| otherElems) right) elemToInsert =
  case compare elemToInsert curElem of
    LT ->
      let newLeft = insert left elemToInsert
       in split $ skew $ AANode height newLeft elems right
    EQ -> AANode height left (elemToInsert :| (curElem : otherElems)) right
    GT ->
      let newRight = insert right elemToInsert
       in split $ skew $ AANode height left elems newRight

data DeleteStrategy
  = DeleteOne
  | DeleteAll
  deriving (Show)

-- | Deletes specified element from tree. If DeleteStrategy is DeleteOne, deletes only one element.
-- Otherwise, deletes all elements, that are equal to the specified.
-- If tree doesn't contain specified element, returns tree unchanged.
-- >>> toList $ delete (fromList [1,7,3,3]) 3 DeleteOne
-- [1,3,7]
-- >>> toList $ delete (fromList [1,7,3,3]) 3 DeleteAll
-- [1,7]
-- >>> toList $ delete (fromList [1,7]) 3 DeleteOne
-- [1,7]
-- >>> toList $ delete (fromList [1,7,3]) 3 DeleteOne
-- [1,7]
delete :: Ord a => AATree a -> a -> DeleteStrategy -> AATree a
delete AAEmpty _ _ = AAEmpty
delete (AANode height left elems@(curElem :| otherElems) right) elemToDelete strategy =
  case compare elemToDelete curElem of
    LT ->
      let newLeft = delete left elemToDelete strategy
       in rebalance $ AANode height newLeft elems right
    GT ->
      let newRight = delete right elemToDelete strategy
       in rebalance $ AANode height left elems newRight
    EQ ->
      case strategy of
        DeleteAll -> deleteAllFromVertex height left right
        DeleteOne ->
          case otherElems of
            []                             -> deleteAllFromVertex height left right
            (otherElemsHead:otherElemsTail) -> AANode height left (otherElemsHead :| otherElemsTail) right

rebalance :: AATree a -> AATree a
rebalance tree = splitRight $ split $ skewRightRight $ skewRight $ skew $ decreaseLevel tree

-- | getMaxFromSubtree :: elems -> right subtree -> max
getMaxFromSubtree :: NonEmpty a -> AATree a -> NonEmpty a
getMaxFromSubtree xs AAEmpty                           = xs
getMaxFromSubtree _ (AANode _ _ rightElems rightRight) = getMaxFromSubtree rightElems rightRight

-- | getMinFromSubtree :: left subtree -> elems -> min
getMinFromSubtree :: AATree a -> NonEmpty a -> NonEmpty a
getMinFromSubtree AAEmpty xs                        = xs
getMinFromSubtree (AANode _ leftLeft leftElems _) _ = getMinFromSubtree leftLeft leftElems

deleteAllFromVertex :: Ord a => Int -> AATree a -> AATree a -> AATree a
deleteAllFromVertex height left right =
  case (left, right) of
    (AAEmpty, AAEmpty) -> AAEmpty
    (AAEmpty, AANode _ rightLeft rightElems _) ->
      let successors@(successorsHead :| _) = getMinFromSubtree rightLeft rightElems
          newRight = delete right successorsHead DeleteAll
       in rebalance $ AANode height left successors newRight
    (AANode _ _ leftElems leftRight, _) ->
      let predecessors@(predecessorsHead :| _) = getMaxFromSubtree leftElems leftRight
          newLeft = delete left predecessorsHead DeleteAll
       in rebalance $ AANode height newLeft predecessors right

skewRight :: AATree a -> AATree a
skewRight AAEmpty                          = AAEmpty
skewRight (AANode height left elems right) = AANode height left elems (skew right)

skewRightRight :: AATree a -> AATree a
skewRightRight (AANode height left elems (AANode rightHeight rightLeft rightElems rightRight)) =
  let newRight = AANode rightHeight rightLeft rightElems (skew rightRight)
   in AANode height left elems newRight
skewRightRight orig = orig

splitRight :: AATree a -> AATree a
splitRight AAEmpty                          = AAEmpty
splitRight (AANode height left elems right) = AANode height left elems (split right)

decreaseLevel :: AATree a -> AATree a
decreaseLevel AAEmpty = AAEmpty
decreaseLevel orig@(AANode height left elements AAEmpty) =
  let shouldBe = 1 + getHeight left
   in if shouldBe < height
        then AANode shouldBe left elements AAEmpty
        else orig
decreaseLevel orig@(AANode height left elements right@(AANode rightHeight rightLeft rightElems rightRight)) =
  let shouldBe = 1 + min (getHeight left) (getHeight right)
   in if shouldBe < height
        then let newRight =
                   if shouldBe < rightHeight
                     then AANode shouldBe rightLeft rightElems rightRight
                     else right
              in AANode shouldBe left elements newRight
        else orig

getHeight :: AATree a -> Int
getHeight AAEmpty               = 0
getHeight (AANode height _ _ _) = height

fromList :: Ord a => [a] -> AATree a
fromList = foldl insert AAEmpty

instance Show a => Show (AATree a) where
  show tree = showLevel tree 0
    where
      showLevel :: Show a => AATree a -> Int -> String
      showLevel AAEmpty level = replicate level '-'
      showLevel (AANode height left (x :| xs) right) level =
        let begining = replicate level '-'
            shownList = "H = " ++ show height ++ " E = " ++ show (x : xs)
            shownLeft = showLevel left (level + 1)
            shownRight = showLevel right (level + 1)
         in begining ++ shownList ++ "\n" ++ shownLeft ++ "\n" ++ shownRight

toList :: AATree a -> [a]
toList = foldr (:) []

find :: Ord a => AATree a -> a -> Maybe (NonEmpty a)
find AAEmpty _ = Nothing
find (AANode _ left elems@(x :| _) right) elemToFind =
  case compare elemToFind x of
    LT -> find left elemToFind
    EQ -> Just elems
    GT -> find right elemToFind

contains :: Ord a => AATree a -> a -> Bool
contains tree elemToFind = not $ null $ find tree elemToFind

isEmpty :: AATree a -> Bool
isEmpty AAEmpty = True
isEmpty _       = False

size :: AATree a -> Int
size AAEmpty                     = 0
size (AANode _ left elems right) = size left + length elems + size right

instance Foldable AATree where
  foldMap :: Monoid m => (a -> m) -> AATree a -> m
  foldMap _ AAEmpty = mempty
  foldMap f (AANode _ left elems right) =
    let leftFolded = foldMap f left
        rightFolded = foldMap f right
        elemsFolded = foldMap f elems
     in leftFolded <> elemsFolded <> rightFolded
  foldr :: (a -> b -> b) -> b -> AATree a -> b
  foldr _ z AAEmpty = z
  foldr f z (AANode _ left elems right) =
    let rightFolded = foldr f z right
        elemsFolded = foldr f rightFolded elems
     in foldr f elemsFolded left
