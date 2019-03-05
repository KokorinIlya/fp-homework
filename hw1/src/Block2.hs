module Block2
  ( deleteByIndex
  , mergeSort
  ) where

-- | if length of list is less then given index, returns element
-- by given index and list without this element, otherwise, returns None
-- >>> deleteByIndex 5 [0..10]
-- Just (5,[0,1,2,3,4,6,7,8,9,10])
-- >>> deleteByIndex 0 [0..10]
-- Just (0,[1,2,3,4,5,6,7,8,9,10])
-- >>> deleteByIndex 10 [0..10]
-- Just (10,[0,1,2,3,4,5,6,7,8,9])
-- >>> deleteByIndex 17 [0..10]
-- Nothing
deleteByIndex :: Int -> [t] -> Maybe (t, [t])
deleteByIndex ind list = deleteByIndexAccum ind list []
  where
    deleteByIndexAccum :: Int -> [t] -> [t] -> Maybe (t, [t])
    deleteByIndexAccum _ [] _ = Nothing
    deleteByIndexAccum k (x:xs) accum
      | k == 0    = Just (x, reverse accum ++ xs)
      | otherwise = deleteByIndexAccum (k - 1) xs (x : accum)

-- | Returns sorted list, O(N * log N) time complexity
-- >>> mergeSort [8,3,1,5]
-- [1,3,5,8]
-- >>> mergeSort []
-- []
-- >>> mergeSort [1,17,5,3,89,-2]
-- [-2,1,3,5,17,89]
-- >>> mergeSort [5,1,-2,4,1]
-- [-2,1,1,4,5]
-- >>> mergeSort [5,7,0,-1,-3,1,5,99,1,1]
-- [-3,-1,0,1,1,1,5,5,7,99]
mergeSort :: Ord t => [t] -> [t]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort list =
  let (first, second) = divide (length list `div` 2) list
   in merge (mergeSort first) (mergeSort second)
  where
    merge :: Ord t => [t] -> [t] -> [t]
    merge xs ys = reverse $ mergeAcc xs ys []

    mergeAcc :: Ord t => [t] -> [t] -> [t] -> [t]
    mergeAcc [] second acc = reverse second ++ acc
    mergeAcc first [] acc  = reverse first ++ acc
    mergeAcc first@(x:xs) second@(y:ys) acc
      | x <= y    = mergeAcc xs second (x : acc)
      | otherwise = mergeAcc first ys (y : acc)

    divide :: Int -> [t] -> ([t], [t])
    divide elementsToCut l =
      let (left, right) = divideAcc elementsToCut [] l
       in (reverse left, right)

    divideAcc :: Int -> [t] -> [t] -> ([t], [t])
    divideAcc _ acc [] = (acc, [])
    divideAcc elementsLeft acc l@(x:xs)
      | elementsLeft > 0 = divideAcc (elementsLeft - 1) (x : acc) xs
      | otherwise        = (acc, l)
