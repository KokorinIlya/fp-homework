module Block1
       (
         order3
       , smartReplicate
       , contains
       , stringSum
       ) where

-- | Returns sorted 3-element tuple
order3 :: Ord t => (t, t, t) -> (t, t, t)
order3 (a, b, c)
  | a <= b && b <= c = (a, b, c)
  | a <= c && c <= b = (a, c, b)
  | b <= a && a <= c = (b, a, c)
  | b <= c && c <= a = (b, c, a)
  | c <= a && a <= b = (c, a, b)
  | otherwise        = (c, b, a)

-- | If list element is equal N, replicates it N times
smartReplicate :: (Num t, Ord t) => [t] -> [t]
smartReplicate list = reverse $ smartReplicateAccum list []
  where
    smartReplicateAccum :: (Num t, Ord t) => [t] -> [t] -> [t]
    smartReplicateAccum [] acc         = acc
    smartReplicateAccum (num:nums) acc =
      smartReplicateAccum nums ((replicateNumTimes num) ++ acc)

    replicateNumTimes :: (Num t, Ord t) => t -> [t]
    replicateNumTimes n = replicateNumTimesAccum n n []

    replicateNumTimesAccum :: (Num t, Ord t) => t -> t -> [t] -> [t]
    replicateNumTimesAccum n k acc
                        | k > 0     = replicateNumTimesAccum n (k - 1) (n:acc)
                        | otherwise = acc

-- | returns only lists, containing specified element
contains :: Eq t => t -> [[t]] -> [[t]]
contains e listOflists = containsAccum e listOflists []
  where
    containsAccum :: Eq t => t -> [[t]] -> [[t]] -> [[t]]
    containsAccum _ [] acc = reverse acc
    containsAccum element (list:lists) acc = if (listContains element list)
      then containsAccum element lists (list:acc)
      else containsAccum element lists acc

    listContains :: Eq t => t -> [t] -> Bool
    listContains _ [] = False
    listContains elementToCheck (x:xs) = if elementToCheck == x
      then True
      else listContains elementToCheck xs

-- | Calculates sum of numbers, separated by whitespace characters
stringSum :: String -> Int
stringSum string = sum $ map read $ words string
