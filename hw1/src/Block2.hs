module Block2
       (
         deleteByIndex
       , mergeSort
       ) where

-- | if length of list is less then given index, returns element
-- by given index and list without this element, otherwise, returns None
deleteByIndex :: (Num it, Eq it) => it -> [t] -> Maybe (t, [t])
deleteByIndex _ [] = Nothing
deleteByIndex k (x:xs) | k == 0    = Just (x, xs)
                       | otherwise = case (deleteByIndex (k - 1) xs) of
                         Nothing -> Nothing
                         Just (deleted, listTail) -> Just (deleted, x:listTail)

-- | Returns sorted list, O(N * log N) time complexity
mergeSort :: Ord t => [t] -> [t]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort list =
  let (first, second) = divide (div (length list) 2) list in
  merge (mergeSort first) (mergeSort second)

  where
    merge :: Ord t => [t] -> [t] -> [t]
    merge [] l = l
    merge l [] = l
    merge first@(x:xs) second@(y:ys) | x <= y = x:(merge xs second)
                                     | otherwise = y:(merge first ys)

    divide :: (Num it, Eq it) => it -> [t] -> ([t], [t])
    divide _ [] = ([], [])
    divide elemsLeft l@(x:xs)
      | elemsLeft == 0 = ([], l)
      | otherwise =
        let (left, right) = divide (elemsLeft - 1) xs in
        ((x:left), right)
