module Task7
       (
         first
       , second
       , third
       ) where

first :: Bool
first = nullAfterHead $ mappedList
  where
    {-
    map :: (a' -> b') -> [a'] -> [b']
    uncurryId :: (b -> c, b) -> c

    a' = (b -> c, b)
    b' = c

    a' = (b -> c, b) = [([Char] -> [Char], [Char])]
    b = c = [Char]
    -}
    mappedList :: [[Char]]
    mappedList = (map :: (a -> b) -> [a] -> [b]) uncurryId list

    {-
    id :: a -> a
    uncurry :: (a' -> b' -> c') -> (a', b) -> c'

    id :: (b -> c) -> (b -> c) = (b -> c) -> b -> c
    (uncurry id) : a' = b -> c, b' = b, c' = c
    uncurry id :: (b -> c, b) -> c
    -}
    uncurryId :: (b -> c, b) -> c
    uncurryId = uncurry id

    list :: [([Char] -> [Char], [Char])]
    list = [(partiallyAppliedPlus, " Grey" :: [Char])]

    partiallyAppliedPlus :: [Char] -> [Char]
    partiallyAppliedPlus = ((++) :: [a] -> [a] -> [a]) ("Dorian " :: [Char])

    nullAfterHead :: Foldable f => [f a] -> Bool
    nullAfterHead = ((.) :: (b -> c) -> (a -> b) -> a -> c)
      (null :: Foldable t => t a -> Bool) (head :: [a] -> a)

third :: Integral a => a -> Bool
third =
    let
      impl :: Bool -> Bool -> Bool
      impl = \x y -> ((||) :: Bool -> Bool -> Bool)
        (((not :: Bool -> Bool) x) :: Bool) (y :: Bool) in
    let
      isMod2 :: Integral a => a -> Bool
      isMod2 = \x -> ((==) :: Eq a => a -> a -> Bool)
        ((mod :: Integral a => a -> a -> a) x (2 :: Integral a => a))
        (0 :: Integral a => a) in
    let
      isMod4 :: Integral a => a -> Bool
      isMod4 = \x -> ((==) :: Eq a => a -> a -> Bool)
        ((mod :: Integral a => a -> a -> a) x (4 :: Integral a => a))
        (0 :: Integral a => a) in
    \x -> ((isMod4 x) :: Bool) `impl` ((isMod2 x) :: Bool)

second = (\x -> zip (lefts x) (rights x)) [Left (1 + 2), Right (2 ^ 6)]
  where
    lefts :: [Either a b] -> [a]
    lefts []                  = []
    lefts ((Left aValue):xs)  = aValue:(lefts xs)
    lefts ((Right _):xs) = lefts xs

    rights :: [Either a b] -> [b]
    rights []                  = []
    rights ((Left _):xs)  = rights xs
    rights ((Right bValue):xs) = bValue:(rights xs)
