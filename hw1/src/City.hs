module City
       (
         City (..)
       , ChurchOrLibrary (..)
       , Family (..)
       , LordName (..)
       , Castle (..)
       , House (..)
       , buildCastle
       , buildChurchOrLibrary
       , buildHouse
       , addLord
       , buildWall
       ) where

import           Data.List.NonEmpty (NonEmpty (..))

data City = City
  { churchOrLibrary :: ChurchOrLibrary
  , houses          :: NonEmpty House
  , castle          :: Castle
  }

data ChurchOrLibrary = Church | Library | Neither
  deriving Show

data Family = Single | Two | Three | Four

newtype House = House Family

newtype LordName = LordName String

data Castle = NoCastle
            | OnlyCastle
            | CastleWithLord LordName
            | CastleWithWalls
            | CastleWithWallsAndLord LordName

buildCastle :: City -> Either String City
buildCastle city = case castle city of
  NoCastle -> Right (city { castle = OnlyCastle })
  _        -> Left "Castle is already built"

buildChurchOrLibrary :: City -> ChurchOrLibrary -> Either String City
buildChurchOrLibrary city toBuild = case churchOrLibrary city of
  Neither ->
   Right city { churchOrLibrary = toBuild }
  x ->
   Left ("Cannot build " ++ show toBuild ++ ", because " ++ show x ++ "is built")

buildHouse :: City -> Family -> City
buildHouse city@City { houses = (house :| otherHouses) } family =
   city { houses = House family :| (house:otherHouses) }

addLord :: City -> LordName -> Either String City
addLord city lord = case castle city of
  NoCastle ->
    Left "Cannot add lord, because house is not built"
  OnlyCastle ->
    Right city { castle = CastleWithLord lord }
  CastleWithLord (LordName lordName) ->
    Left ("Lord " ++ lordName ++ " is already present")
  CastleWithWalls ->
    Right city { castle = CastleWithWallsAndLord lord }
  CastleWithWallsAndLord (LordName lordName) ->
    Left ("Lord " ++ lordName ++ " is already present")

buildWall :: City -> Either String City
buildWall city@City { houses = house :| otherHouses, castle = cityCastle } =
  if moreThan10People
  then case cityCastle of
    NoCastle -> Left "Cannot build a wall, because castle isn't built"
    OnlyCastle -> Right city { castle = CastleWithWalls }
    CastleWithLord lord -> Right city {castle = CastleWithWallsAndLord lord}
    _ -> Left "Walls already built"

  else Left "Less then 10 people"
    where
      moreThan10People :: Bool
      moreThan10People = peopleCountAcc (house:otherHouses) 0 > 10

      peopleCountAcc :: [House] -> Int -> Int
      peopleCountAcc [] acc = acc
      peopleCountAcc (House family : housesTail) acc =
        peopleCountAcc housesTail (countPeople family + acc)

      countPeople :: Family -> Int
      countPeople Single   = 1
      countPeople Two      = 2
      countPeople Three    = 3
      countPeople Four     = 4
