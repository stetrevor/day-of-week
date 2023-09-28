module DayOfWeek where

import Data.Monoid

data Date a = Date { y :: a, m :: a, d :: a }
  deriving Show

isLeap :: Integral a => a -> Bool
isLeap a = a `mod` 4 == 0 && a `mod` 100 /= 0 || a `mod` 400 == 0

class Count c where
  count :: Num a => c -> Sum a

data Year a = LeapYear { num :: a } | NormalYear { num :: a }
  deriving Show

instance Integral a => Count (Year a) where
  count a
    | num a == 1970 = mempty
    | otherwise = if isLeap (num a) then Sum 366 else Sum 365

data Month a = LeapMonth a | NormalMonth a
  deriving Show

instance (Num a, Eq a) => Count (Month a) where
  count (LeapMonth 1) = mempty
  count (LeapMonth 2) = Sum 31
  count (LeapMonth 3) = Sum 29
  count (LeapMonth 4) = Sum 31
  count (LeapMonth 5) = Sum 30
  count (LeapMonth 6) = Sum 31
  count (LeapMonth 7) = Sum 30
  count (LeapMonth 8) = Sum 31
  count (LeapMonth 9) = Sum 31
  count (LeapMonth 10) = Sum 30
  count (LeapMonth 11) = Sum 31
  count (LeapMonth 12) = Sum 30
  count (NormalMonth 1) = mempty
  count (NormalMonth 2) = Sum 31
  count (NormalMonth 3) = Sum 28
  count (NormalMonth 4) = Sum 31
  count (NormalMonth 5) = Sum 30
  count (NormalMonth 6) = Sum 31
  count (NormalMonth 7) = Sum 30
  count (NormalMonth 8) = Sum 31
  count (NormalMonth 9) = Sum 31
  count (NormalMonth 10) = Sum 30
  count (NormalMonth 11) = Sum 31
  count (NormalMonth 12) = Sum 30

date :: a -> a -> a -> Date a
date y m d = Date { y, m, d }

year :: Integral a => a -> Year a
year y = if isLeap y then LeapYear y else NormalYear y

month :: Integral a => a -> a -> Month a
month y ord = if isLeap y then LeapMonth ord else NormalMonth ord

days :: Integral a => Date a -> Sum a
days (Date { y, m, d }) = mconcat (map (count . year) [1970..y]) <> mconcat (map (count . month y) [1..m]) <> Sum (d - 1)

data WeekDay = Thursday | Friday | Saturday | Sunday | Monday | Tuesday | Wednesday
  deriving (Show, Enum)

dayofweek :: Date Int -> WeekDay
dayofweek = toEnum . (`mod` 7) . getSum . days