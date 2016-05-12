module OpeningHours
( isOpen
, nextOpen
, Shop (..)
, WeekDay (..)
) where

import Data.Time
import Data.List (find)
import Data.Maybe (fromJust)

data WeekDay = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Eq, Show, Read)

data Shop = Shop { openingDays :: [WeekDay], openingHours :: (DiffTime,DiffTime) } deriving (Show)

isOpen :: Shop -> UTCTime -> Bool
isOpen (Shop daysOpen (openingTime,closingTime)) (UTCTime date time) =
    let checkDay = date `inWeekDays` daysOpen
        checkTime = time >= openingTime && time < closingTime
    in  checkDay && checkTime

nextOpen :: Shop -> UTCTime -> UTCTime
nextOpen (Shop daysOpen (openingTime,_)) (UTCTime startDate startTime) =
    let followingDays = map (`addDays` startDate) [1..]
        nextWorkingDay = fromJust $ find (`inWeekDays` daysOpen) followingDays
    in  if startTime < openingTime && startDate `inWeekDays` daysOpen
        then UTCTime startDate openingTime
        else UTCTime nextWorkingDay openingTime

inWeekDays :: Day -> [WeekDay] -> Bool
day `inWeekDays` weekDays = readDay day `elem` weekDays

readDay :: Day -> WeekDay
readDay = read . formatTime defaultTimeLocale "%a"
