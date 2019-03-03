module Task7
  ( first
  , second
  , third
  ) where

third :: Integer -> Bool
third =
  let impl :: Bool -> Bool -> Bool
      impl =
        \x y ->
          ((((||) :: Bool -> Bool -> Bool) ((not :: Bool -> Bool) (x :: Bool) :: Bool) :: Bool -> Bool) (y :: Bool) :: Bool)
   in let isMod2 :: Integer -> Bool
          isMod2 =
            \x ->
              ((((==) :: Integer -> Integer -> Bool)
                  (((mod :: Integer -> Integer -> Integer) (x :: Integer) :: Integer -> Integer) (2 :: Integer) :: Integer) :: Integer -> Bool)
                 (0 :: Integer) :: Bool)
       in let isMod4 :: Integer -> Bool
              isMod4 =
                \x ->
                  ((((==) :: Integer -> Integer -> Bool)
                      (((mod :: Integer -> Integer -> Integer) (x :: Integer) :: Integer -> Integer) (4 :: Integer) :: Integer) :: Integer -> Bool)
                     (0 :: Integer) :: Bool)
           in \x ->
                (((impl :: Bool -> Bool -> Bool) ((isMod4 :: Integer -> Bool) (x :: Integer) :: Bool) :: Bool -> Bool)
                   ((isMod2 :: Integer -> Bool) (x :: Integer) :: Bool) :: Bool)

-- a1 == [t a]
-- b1 == Bool
first :: Bool
first = ((($) :: (a1 -> b1) -> a1 -> b1) (nullAfterHead :: Foldable t => [t a] -> Bool) :: Foldable t => [t a] -> Bool) (mappedList :: [String])
  where
    -- a1 == (a -> b, a)
    -- b1 == b
    mappedList :: [String]
    mappedList = ((map :: (a1 -> b1) -> [a1] -> [b1]) (uncId :: (a -> b, a) -> b) :: [(a -> b, a)] -> [b]) (list :: [(String -> String, String)])

    -- a1 == a -> b
    -- b1 == a
    -- c1 == b
    uncId :: (a -> b, a) -> b
    uncId = (uncurry :: (a1 -> b1 -> c1) -> (a1, b1) -> c1) (id :: (a -> b) -> a -> b)

    list :: [(String -> String, String)]
    list = [(((++) :: [a] -> [a] -> [a]) ("Dorian " :: String) :: String -> String, " Grey" :: String)]

    -- b1 == t a
    -- c1 == Bool
    -- a1 == [d]
    -- t a == d
    -- => a1 == [d] == [t a]
    nullAfterHead :: Foldable t => [t a] -> Bool
    nullAfterHead = (((.) :: (b1 -> c1) -> (a1 -> b1) -> a1 -> c1) (null :: Foldable t => t a -> Bool) :: Foldable t => (a1 -> t a) -> a1 -> Bool) (head :: [d] -> d)

second :: [(Integer, Integer)]
second = (f :: [Either Integer Integer] -> [(Integer, Integer)]) (list :: [Either Integer Integer])
  where
    f :: [Either Integer Integer] -> [(Integer, Integer)]
    f = \x -> ((zip :: [Integer] -> [Integer] -> [(Integer, Integer)]) (lefts x :: [Integer]) :: [Integer] -> [(Integer, Integer)]) (rights x :: [Integer])

    list :: [Either Integer Integer]
    list = [left :: Either Integer b, right :: Either a Integer]

    left :: Either Integer b
    left = (Left :: a -> Either a b) (firstNum :: Integer)

    right :: Either a Integer
    right = (Right :: b -> Either a b) (secondNum :: Integer)

    firstNum :: Integer
    firstNum = (((+) :: Integer -> Integer -> Integer) (1 :: Integer) :: Integer -> Integer) (2 :: Integer)

    secondNum :: Integer
    secondNum = (((^) :: Integer -> Integer -> Integer) (2 :: Integer) :: Integer -> Integer) (6 :: Integer)

    lefts :: [Either a b] -> [a]
    lefts x = [a | Left a <- x]

    rights :: [Either a b] -> [b]
    rights x = [b | Right b <- x]
