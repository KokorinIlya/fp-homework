module City
  ( City(..)
  , ChurchOrLibrary(..)
  , Family(..)
  , LordName(..)
  , Castle(..)
  , House(..)
  , CastleBuildingError(..)
  , Walls(..)
  , AddLordError(..)
  , BuildWallsError(..)
  , BuildChurchOrLibraryError(..)
  , buildCastle
  , buildChurchOrLibrary
  , buildHouse
  , addLord
  , buildWall
  ) where

import           NonEmptys (NonEmpty (..))

data City = City
  { churchOrLibrary :: ChurchOrLibrary
  , houses          :: NonEmpty House
  , castle          :: Maybe Castle
  } deriving (Show)

instance Eq City where
  first == second =
    (churchOrLibrary first == churchOrLibrary second) &&
    (houses first == houses second) && (castle first == castle second)

data ChurchOrLibrary
  = Church
  | Library
  | Neither
  deriving (Show)

instance Eq ChurchOrLibrary where
  Church == Church   = True
  Library == Library = True
  Neither == Neither = True
  _ == _             = False

data Family
  = Single
  | Two
  | Three
  | Four
  deriving (Show)

instance Eq Family where
  Single == Single = True
  Two == Two       = True
  Three == Three   = True
  Four == Four     = True
  _ == _           = False

newtype House =
  House Family
  deriving (Show)

instance Eq House where
  (House firstFamily) == (House secondFamily) = firstFamily == secondFamily

newtype LordName =
  LordName String
  deriving (Show)

instance Eq LordName where
  (LordName firstLordName) == (LordName secondLordName) = firstLordName == secondLordName

data Walls =
  Walls
  deriving (Show)

instance Eq Walls where
  Walls == Walls = True

data Castle = Castle
  { lord  :: Maybe LordName
  , walls :: Maybe Walls
  } deriving (Show)

instance Eq Castle where
  firstCastle == secondCastle = (lord firstCastle == lord secondCastle) && (walls firstCastle == walls secondCastle)

data CastleBuildingError =
  CastleIsAlreadyBuild
  deriving (Show)

instance Eq CastleBuildingError where
  CastleIsAlreadyBuild == CastleIsAlreadyBuild = True

-- | Try to build castle in the city, returns city with castle with city,
-- if successful, CastleBuildingError otherwise
-- >>> let cityWithoutCastle = City { churchOrLibrary = Church, houses = House Single :| [], castle = Nothing}
-- >>> let someCastle = Castle { lord = Nothing, walls = Just Walls }
-- >>> buildCastle cityWithoutCastle someCastle == Right cityWithoutCastle{ castle = Just someCastle }
-- True
-- >>> let cityWithCastle = City { churchOrLibrary = Church, houses = House Single :| [], castle = Just someCastle}
-- >>> buildCastle cityWithCastle someCastle == Left CastleIsAlreadyBuild
-- True
buildCastle :: City -> Castle -> Either CastleBuildingError City
buildCastle city newCastle =
  case castle city of
    Nothing -> Right (city {castle = Just newCastle})
    Just _  -> Left CastleIsAlreadyBuild

data BuildChurchOrLibraryError
  = LibraryAlreadyBuilt
  | ChurchAlreadyBuilt
  deriving (Show)

instance Eq BuildChurchOrLibraryError where
  LibraryAlreadyBuilt == LibraryAlreadyBuilt = True
  ChurchAlreadyBuilt == ChurchAlreadyBuilt   = True
  _ == _                                     = False

-- | Builds church or library, if neither church nor library has been built
-- >>> let cityWithout = City { churchOrLibrary = Neither, houses = House Single :| [], castle = Nothing}
-- >>> buildChurchOrLibrary cityWithout Church == Right cityWithout { churchOrLibrary = Church}
-- True
-- >>> let cityWithChurch = City { churchOrLibrary = Church, houses = House Single :| [], castle = Nothing}
-- >>> buildChurchOrLibrary cityWithChurch Library == Left ChurchAlreadyBuilt
-- True
-- >>> let cityWithLibrary = City { churchOrLibrary = Library, houses = House Single :| [], castle = Nothing}
-- >>> buildChurchOrLibrary cityWithLibrary Library == Left LibraryAlreadyBuilt
-- True
buildChurchOrLibrary :: City -> ChurchOrLibrary -> Either BuildChurchOrLibraryError City
buildChurchOrLibrary city toBuild =
  case churchOrLibrary city of
    Neither -> Right city {churchOrLibrary = toBuild}
    Church  -> Left ChurchAlreadyBuilt
    Library -> Left LibraryAlreadyBuilt

-- | Returns city with one more house for a given family built
buildHouse :: City -> Family -> City
buildHouse city@City {houses = (house :| otherHouses)} family = city {houses = House family :| (house : otherHouses)}

data AddLordError
  = LordIsAlreadyPresent LordName
  | CannotAddLordNeedCastle
  deriving (Show)

instance Eq AddLordError where
  (LordIsAlreadyPresent firstLordName) == (LordIsAlreadyPresent secondLordName) = firstLordName == secondLordName
  CannotAddLordNeedCastle == CannotAddLordNeedCastle                            = True
  _ == _                                                                        = False

-- | Add lord to the city, if there is a castle, and no lord.
-- >>> let cityHouses = House Single :| []
-- >>> let cityWithoutCastle = City { churchOrLibrary = Church, houses = cityHouses, castle = Nothing }
-- >>> addLord cityWithoutCastle (LordName "lord") == Left CannotAddLordNeedCastle
-- True
-- >>> let castleWithLord = Castle { lord = Just $ LordName "another lord", walls = Nothing }
-- >>> let cityWithLord = City { churchOrLibrary = Church, houses = cityHouses, castle = Just castleWithLord }
-- >>> addLord cityWithLord (LordName "lord") == Left (LordIsAlreadyPresent (LordName "another lord"))
-- True
-- >>> let cityCastle = Castle { lord = Nothing, walls = Nothing }
-- >>> let city = City { churchOrLibrary = Church, houses = cityHouses, castle = Just cityCastle }
-- >>> let newCastle = cityCastle { lord = Just $ LordName "lord" }
-- >>> addLord city (LordName "lord") == Right (city { castle = Just newCastle })
-- True
addLord :: City -> LordName -> Either AddLordError City
addLord city newLord =
  case castle city of
    Nothing -> Left CannotAddLordNeedCastle
    (Just cityCastle@Castle {lord = Nothing}) -> Right city {castle = Just $ cityCastle {lord = Just newLord}}
    (Just Castle {lord = Just cityLord}) -> Left $ LordIsAlreadyPresent cityLord

data BuildWallsError
  = CannotBuildWallsNeedCastle
  | LessThen10People
  | AlreadyHasWalls
  deriving (Show)

instance Eq BuildWallsError where
  CannotBuildWallsNeedCastle == CannotBuildWallsNeedCastle = True
  LessThen10People == LessThen10People                     = True
  AlreadyHasWalls == AlreadyHasWalls                       = True
  _ == _                                                   = False

-- | Makes city great again and builds walls, if possible
-- >>> let cityHouses = House Four :| [House Four, House Four]
-- >>> let cityWithoutCastle = City { churchOrLibrary = Church, houses = cityHouses, castle = Nothing }
-- >>> buildWall cityWithoutCastle == Left CannotBuildWallsNeedCastle
-- True
-- >>> let castleWithWalls = Castle { walls = Just Walls, lord = Nothing }
-- >>> let cityWithWalls= City { churchOrLibrary = Church, houses = cityHouses, castle = Just castleWithWalls }
-- >>> buildWall cityWithWalls == Left AlreadyHasWalls
-- True
-- >>> smallHouses = House Three :| []
-- >>> let castleWithoutWalls = Castle { walls = Nothing, lord = Nothing }
-- >>> let smallCity = City { churchOrLibrary = Church, houses = smallHouses, castle = Just castleWithoutWalls }
-- >>> buildWall smallCity == Left LessThen10People
-- True
-- >>> let normalCity = City { churchOrLibrary = Church, houses = cityHouses, castle = Just castleWithoutWalls }
-- >>> let newCastle = castleWithoutWalls {walls = Just Walls }
-- >>> buildWall normalCity == Right (normalCity { castle = Just newCastle } )
-- True
buildWall :: City -> Either BuildWallsError City
buildWall city@City {houses = house :| otherHouses, castle = maybeCastle} =
  if atLeast10People
    then case maybeCastle of
           Nothing -> Left CannotBuildWallsNeedCastle
           (Just cityCastle@Castle {walls = Nothing}) -> Right city {castle = Just $ cityCastle {walls = Just Walls}}
           (Just Castle {walls = Just _}) -> Left AlreadyHasWalls
    else Left LessThen10People
  where
    atLeast10People :: Bool
    atLeast10People = peopleCountAcc (house : otherHouses) 0 >= 10
    peopleCountAcc :: [House] -> Int -> Int
    peopleCountAcc [] acc = acc
    peopleCountAcc (House family:housesTail) acc = peopleCountAcc housesTail (countPeople family + acc)
    countPeople :: Family -> Int
    countPeople Single = 1
    countPeople Two    = 2
    countPeople Three  = 3
    countPeople Four   = 4
