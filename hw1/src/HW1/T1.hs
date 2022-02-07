module HW1.T1
  ( Day (..)
  , afterDays
  , daysToParty
  , isWeekend
  , nextDay
  ) where

import Numeric.Natural (Natural)

data Day 
  = Monday 
  | Tuesday 
  | Wednesday 
  | Thursday 
  | Friday 
  | Saturday 
  | Sunday deriving (Show, Eq)

nextDay :: Day -> Day
nextDay Monday = Tuesday
nextDay Tuesday = Wednesday
nextDay Wednesday = Thursday
nextDay Thursday = Friday
nextDay Friday = Saturday
nextDay Saturday = Sunday
nextDay Sunday = Monday

afterDays :: Natural -> Day -> Day
afterDays 0 day = day
afterDays n day = afterDays (n - 1) (nextDay day)

isWeekend :: Day -> Bool
isWeekend day = case day of
  Saturday -> True
  Sunday -> True
  _ -> False

daysToParty :: Day -> Natural
daysToParty Friday = 0
daysToParty day = (+ 1) $ daysToParty $ nextDay day
