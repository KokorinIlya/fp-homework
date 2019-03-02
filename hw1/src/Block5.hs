module Block5
  ( maybeConcat
  , eitherConcat
  , ThisOrThat(..)
  , Name(..)
  , Builder(..)
  , fromString
  , fastConcat
  , toString
  ) where

maybeConcat :: (Monoid m, Foldable t) => t (Maybe m) -> m
maybeConcat = foldMap mapToMonad
  where
    mapToMonad :: Monoid m => Maybe m -> m
    mapToMonad Nothing  = mempty
    mapToMonad (Just x) = x

eitherConcat :: (Foldable t, Monoid l, Monoid r) => t (Either l r) -> (l, r)
eitherConcat = foldMap processElem
  where
    processElem :: (Monoid l, Monoid r) => Either l r -> (l, r)
    processElem (Left lValue)  = (lValue, mempty)
    processElem (Right rValue) = (mempty, rValue)

data ThisOrThat a b
  = This a
  | That b
  | Both a
         b
  deriving (Show)

instance Semigroup (ThisOrThat a b) where
  This _ <> second@(This _)     = second
  This x <> That y              = Both x y
  This _ <> second@(Both _ _)   = second
  That x <> This y              = Both y x
  That _ <> second@(That _)     = second
  That _ <> second@(Both _ _)   = second
  Both _ y <> This z            = Both z y
  Both x _ <> That z            = Both x z
  Both _ _ <> second@(Both _ _) = second

newtype Name =
  Name String
  deriving (Show)

instance Semigroup Name where
  Name first <> Name second
    | first == ""  = Name second
    | second == "" = Name first
    | otherwise    = Name (first ++ "." ++ second)

instance Monoid Name where
  mempty = Name ""

newtype Endo a = Endo
  { getEndo :: a -> a
  }

instance Semigroup (Endo a) where
  Endo {getEndo = firstEndo} <> Endo {getEndo = secondEndo} = Endo {getEndo = firstEndo . secondEndo}

instance Monoid (Endo a) where
  mempty = Endo {getEndo = id}

data Builder
  = One Char
  | Many [Builder]
  deriving (Show, Eq)

instance Semigroup Builder where
  x <> Many []                                  = x
  Many [] <> x                                  = x
  first@(One _) <> second@(One _)               = Many [first, second]
  first@(One _) <> Many (builder:builders)      = Many (first : builder : builders)
  Many list@(_:_) <> first@(One _)              = Many (list ++ [first])
  Many firstList@(_:_) <> Many secondList@(_:_) = Many (firstList ++ secondList)

instance Monoid Builder where
  mempty = Many []

fromString :: String -> Builder
fromString = foldMap One

-- | Gives weaker post-conditions, than <> operator, because
-- One 'a' `fastConcat` (Many [One 'b', One 'c']
-- `fastConcat` Many [One 'd', One 'e']) != (One 'a' `fastConcat`
-- Many [One 'b', One 'c']) `fastConcat` Many [One 'd', One 'e'],
-- but it is faster. Post-condition is :
-- toString $ x `fastConcat` (y `fastConcat` z) ==
-- toString $ (x `fastConcat` y) `fastConcat` z
fastConcat :: Builder -> Builder -> Builder
fastConcat x (Many [])                               = x
fastConcat (Many []) x                               = x
fastConcat first@(One _) (Many (builder : builders)) = Many (first : builder : builders)
fastConcat x y                                       = Many [x, y]

toString :: Builder -> String
toString (One c)     = [c]
toString (Many list) = foldMap id (map toString list)
