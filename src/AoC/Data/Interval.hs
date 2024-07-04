{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}

module AoC.Data.Interval
  ( Interval (..),
    IntervalLit (..),
  )
where

{- Typeclass representing data covering some interval from `intervalStart` to `intervalEnd`,
 - where `intervalStart` and `intervalEnd` are comparable values calculated from some value of
 - type `a`.
 -}
class Interval key a | a -> key where
  intervalStart :: (Ord key) => a -> key
  intervalEnd :: (Ord key) => a -> key

  contains :: (Ord key) => a -> key -> Bool
  contains interval value =
    value >= intervalStart interval
      && value <= intervalEnd interval

  containedBy :: (Ord key) => key -> a -> Bool
  containedBy = flip contains

  covers :: (Ord key) => a -> a -> Bool
  covers i1 i2 = intervalStart i1 <= intervalStart i2 && intervalEnd i1 >= intervalEnd i2

  coveredBy :: (Ord key) => a -> a -> Bool
  coveredBy = flip covers

  touches :: (Ord key) => a -> a -> Bool
  touches i1 i2 =
    -- if either interval covers the other, they touch
    i1 `covers` i2
      || i1 `coveredBy` i2
      -- or if interval 1 includes the start of interval 2
      || intervalStart i1 <= intervalStart i2 && intervalEnd i1 >= intervalStart i2
      -- or if interval 1 includes the end of interval 2
      || intervalStart i1 <= intervalEnd i2 && intervalEnd i1 >= intervalEnd i2

data IntervalLit a = IntervalLit
  { start :: a,
    end :: a
  }
  deriving (Eq, Ord, Show)

instance (Ord a) => Interval a (IntervalLit a) where
  intervalStart :: (Ord a) => IntervalLit a -> a
  intervalStart = start
  intervalEnd :: (Ord a) => IntervalLit a -> a
  intervalEnd = end
