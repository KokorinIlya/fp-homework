{-# LANGUAGE MultiWayIf #-}

module Days
  ( Day(..)
  , nextDays
  , isWeekend
  , afterDays
  , daysToParty
  ) where

data Day
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Show)

instance Enum Day where
  toEnum num =
    let dayNum = (num `mod` 7)
     in if | dayNum == 0 -> Monday
           | dayNum == 1 -> Tuesday
           | dayNum == 2 -> Wednesday
           | dayNum == 3 -> Thursday
           | dayNum == 4 -> Friday
           | dayNum == 5 -> Saturday
           | otherwise -> Sunday

  fromEnum Monday    = 0
  fromEnum Tuesday   = 1
  fromEnum Wednesday = 2
  fromEnum Thursday  = 3
  fromEnum Friday    = 4
  fromEnum Saturday  = 5
  fromEnum Sunday    = 6

nextDays :: Day -> Day
nextDays = succ

afterDays :: Day -> Int -> Day
afterDays day k =
  let dayNumber = fromEnum day
   in let afterDayNumber = (dayNumber + k) `mod` 7
       in toEnum afterDayNumber

isWeekend :: Day -> Bool
isWeekend Saturday = True
isWeekend Sunday   = True
isWeekend _        = False

daysToParty :: Day -> Int
daysToParty Monday    = 4
daysToParty Tuesday   = 3
daysToParty Wednesday = 2
daysToParty Thursday  = 1
daysToParty Friday    = 0
daysToParty Saturday  = 6
daysToParty Sunday    = 5
