module Block5
  ( Builder(..)
  , Endo(..)
  , Name(..)
  , ThisOrThat(..)
  , eitherConcat
  , fastConcat
  , fromString
  , maybeConcat
  , toString
  ) where

-- | Concats list (or another foldable structure) of maybe, returning monoid composition of elements
-- inside Just, without Nothing
-- >>> maybeConcat [Just [1,2,3], Nothing, Just [4,5]]
-- [1,2,3,4,5]
maybeConcat :: (Monoid m, Foldable t) => t (Maybe m) -> m
maybeConcat = foldMap mapToMonad
  where
    mapToMonad :: Monoid m => Maybe m -> m
    mapToMonad Nothing  = mempty
    mapToMonad (Just x) = x

-- | Converts some foldable structure, containing Either l r to combination
-- of monoidal composition of all lefts and rights
-- >>> eitherConcat [Left "aba", Right [1,2,3], Left "caba", Right [4,5]]
-- ("abacaba",[1,2,3,4,5])
eitherConcat :: (Foldable t, Monoid l, Monoid r) => t (Either l r) -> (l, r)
eitherConcat = foldMap processElem
  where
    processElem :: (Monoid l, Monoid r) => Either l r -> (l, r)
    processElem (Left lValue)  = (lValue, mempty)
    processElem (Right rValue) = (mempty, rValue)

data ThisOrThat a b
  = This a
  | That b
  | Both a b
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

data Name
  = Name String
  | EmptyName
  deriving (Show)

instance Semigroup Name where
  Name first <> Name second    = Name $ first ++ ('.' : second)
  EmptyName <> second          = second
  first <> EmptyName           = first

instance Monoid Name where
  mempty = EmptyName

newtype Endo a = Endo
  { getEndo :: a -> a
  }

instance Semigroup (Endo a) where
  Endo first <> Endo second = Endo $ first . second

instance Monoid (Endo a) where
  mempty = Endo id

data Builder
  = One Char
  | Many [Builder]
  deriving (Show)

instance Eq Builder where
  One firstChar == One secondChar   = firstChar == secondChar
  One _ == Many _                   = False
  Many _ == One _                   = False
  Many firstList == Many secondList = firstList == secondList

instance Semigroup Builder where
  x <> Many []                                  = x
  Many [] <> x                                  = x
  first@(One _) <> second@(One _)               = Many [first, second]
  first@(One _) <> Many list@(_ : _)            = Many (first : list)
  Many list@(_ : _) <> first@(One _)            = Many (list ++ [first])
  Many firstList@(_:_) <> Many secondList@(_:_) = Many (firstList ++ secondList)

instance Monoid Builder where
  mempty = Many []

-- | Converts string to builder
fromString :: String -> Builder
fromString = foldr addCharToBuilder mempty
  where
    addCharToBuilder :: Char -> Builder -> Builder
    addCharToBuilder chr curBuilder = One chr <> curBuilder

-- | Gives weaker post-conditions, than <> operator, because
-- One 'a' `fastConcat` (Many [One 'b', One 'c']
-- `fastConcat` Many [One 'd', One 'e']) /= (One 'a' `fastConcat`
-- Many [One 'b', One 'c']) `fastConcat` Many [One 'd', One 'e'],
-- but it is faster. Post-condition is :
-- toString $ x `fastConcat` (y `fastConcat` z) ==
-- toString $ (x `fastConcat` y) `fastConcat` z
-- >>> let strings = map fromString ["aba","caba","swsw","wadada","","awdawdwa","cbfhrh","whwdudw","x"]
-- >>> let stringTriples = [(x, y, z) | x <- strings, y <- strings, z <- strings]
-- >>> filter (\(a, b, c) -> (a <> b) <> c /= a <> (b <> c)) stringTriples
-- []
-- >>> filter (\(a, b, c) -> toString ((a `fastConcat` b) `fastConcat` c) /= toString (a `fastConcat` (b `fastConcat` c))) stringTriples
-- []
-- >>> filter (\(a, b, c) -> (a `fastConcat` b) `fastConcat` c /=  a `fastConcat` (b `fastConcat` c)) stringTriples /= []
-- True
-- >>> One 'a' `fastConcat` (Many [One 'b', One 'c'] `fastConcat` Many [One 'd', One 'e']) /= (One 'a' `fastConcat` Many [One 'b', One 'c']) `fastConcat` Many [One 'd', One 'e']
-- True
-- >>> toString (One 'a' `fastConcat` (Many [One 'b', One 'c'] `fastConcat` Many [One 'd', One 'e'])) == toString((One 'a' `fastConcat` Many [One 'b', One 'c']) `fastConcat` Many [One 'd', One 'e'])
-- True
fastConcat :: Builder -> Builder -> Builder
fastConcat x (Many [])                             = x
fastConcat (Many []) x                             = x
fastConcat first@(One _) (Many (builder:builders)) = Many (first : builder : builders)
fastConcat x y                                     = Many [x, y]

-- | Converts builder to string
-- >>> let strings = ["aba","caba","swsw","wadada","","awdawdwa","cbfhrh","whwdudw","x"]
-- >>> filter (\s -> toString (fromString s) /= s) strings
-- []
toString :: Builder -> String
toString (One c)     = [c]
toString (Many list) = foldMap id (map toString list)
