{-# LANGUAGE NamedFieldPuns #-}

module AoC.Data.Interval where

data Interval a = Interval
  { start :: a,
    end :: a
  }
  deriving (Eq, Ord, Show)

-- | Check whether an element is contained in an interval
-- This function *includes both endpoints*.
elem:: (Ord a) => a -> Interval a -> Bool
elem value (Interval {start, end}) = value >= start && value <= end

touches :: (Ord a) => Interval a -> Interval a -> Bool
touches i1@(Interval {start = s1, end = e1}) i2@(Interval {start = s2, end = e2}) =
  i1 `covers` i2
    || i2 `covers` i1
    || (s1 >= s2 && s1 <= e2)
    || (s2 >= s1 && s2 <= e1)

covers :: (Ord a) => Interval a -> Interval a -> Bool
covers (Interval {start = s1, end = e1}) (Interval {start = s2, end = e2}) =
  s1 <= s2 && e1 >= e2
