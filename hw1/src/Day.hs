{-# LANGUAGE MultiWayIf #-}

module Day
  ( Day(..)
  , afterDays
  , daysToParty
  , isWeekend
  , nextDays
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

instance Eq Day where
  Monday == Monday       = True
  Tuesday == Tuesday     = True
  Wednesday == Wednesday = True
  Thursday == Thursday   = True
  Friday == Friday       = True
  Saturday == Saturday   = True
  Sunday == Sunday       = True
  _ == _                 = False

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

-- | Finds next day
-- >>> nextDays Wednesday
-- Thursday
-- >>> nextDays Sunday
-- Monday
nextDays :: Day -> Day
nextDays = succ

-- | Finds day after some days
-- >>> filter (\d -> afterDays d 1 /= nextDays d) (map toEnum [0..6])
-- []
-- >>> let allDays = map toEnum [0..6]
-- >>> map (\d -> afterDays d 7000000) allDays == allDays
-- True
-- >>> afterDays Sunday 7000001
-- Monday
afterDays :: (Integral t) => Day -> t -> Day
afterDays day k =
  let dayNumber = fromEnum day
      daysWithoutWeeks = k `mod` 7
      afterDayNumber = (dayNumber + fromEnum daysWithoutWeeks) `mod` 7
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
