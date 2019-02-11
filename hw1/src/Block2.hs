module Block2
       (
         deleteByIndex
       , mergeSort
       ) where

-- | if length of list is less then given index, returns element
-- by given index and list without this element, otherwise, returns None
deleteByIndex :: (Num it, Eq it) => it -> [t] -> Maybe (t, [t])
deleteByIndex ind list = deleteByIndexAccum ind list []
  where
    deleteByIndexAccum :: (Num it, Eq it) => it -> [t] -> [t] -> Maybe (t, [t])
    deleteByIndexAccum _ [] _ = Nothing
    deleteByIndexAccum k (x:xs) accum
      | k == 0 = Just (x, (reverse accum) ++ xs)
      | otherwise = deleteByIndexAccum (k - 1) xs (x:accum)


-- | Returns sorted list, O(N * log N) time complexity
mergeSort :: Ord t => [t] -> [t]
mergeSort []  = []
mergeSort [x] = [x]
mergeSort list =
  let (first, second) = divide (div (length list) 2) list in
  merge (mergeSort first) (mergeSort second)

  where
    merge :: Ord t => [t] -> [t] -> [t]
    merge xs ys = reverse $ mergeAcc xs ys []

    mergeAcc :: Ord t => [t] -> [t] -> [t] -> [t]
    mergeAcc [] l acc = (reverse l) ++ acc
    mergeAcc l [] acc = (reverse l) ++ acc
    mergeAcc first@(x:xs) second@(y:ys) acc
      | x <= y = mergeAcc xs second (x:acc)
      | otherwise = mergeAcc first ys (y:acc)

    divide :: (Num it, Eq it) => it -> [t] -> ([t], [t])
    divide elementsToCut l =
      let (left, right) = divideAcc elementsToCut [] l in
      (reverse left, right)

    divideAcc :: (Num it, Eq it) => it -> [t] -> [t] -> ([t], [t])
    divideAcc _ acc [] = (acc, [])
    divideAcc elementsLeft acc l@(x:xs)
      | elementsLeft == 0 = (acc, l)
      | otherwise = divideAcc (elementsLeft - 1) (x:acc) xs
