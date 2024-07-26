{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}

module AoC.Data.IntervalTree
  ( singleton,
    empty,
    findCoveringInterval,
    fromList,
    at,
    IntervalTree (..),
  )
where

-- - then:
--   - at is "find key, get all the data"
--   - atInterval is "find keys, get all the (unique) data"
--   - merge is find keys that touch, split them up to find the new keys, but if present in only one of the two trees
--     leave it
--   - insert is find keys, add to values
--   - delete is find keys, remove from values (and maybe merge adjacent?)

import AoC.Data.Interval (Interval (..), IntervalLit (..), ofInterval)
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (fold, foldl')
import Data.List (sort, sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import qualified Data.Vector as Vector

-- Desired operations:
-- for some value, find any intervals in the tree that contain it, and _fast_
-- for some range, find any intervals in the tree that touch it
-- for some range, find any intervals in the tree that cover it
-- consider intervals like:
--                  a  b c
-- interval 1:      --------
-- interval 2:         -----
-- interval 3:   --------
-- what's the natural sort order in the tree such that
-- a query at a conveniently finds 1 and 3, a query at b finds everything, and a query at c finds 1 and 2?
-- can I split by start before/start during/start after? then:
-- query at t is: start at root
-- if start of root is after t, follow "start before" to the left
-- if start of root is before t and end of root is before t, follow "start after" to the right
-- if root contains t, return root + any intervals in `containedBy` root that contain t

-- TODO
-- maybe this needs to track an index as well of node indices to start / end values,
-- so that to find some k I can just bisection search in the index to discover if it's present,
-- then explore nodes from the edges of that node?
data IntervalTree k a = IntervalTree
  { index :: Vector.Vector (IntervalLit k),
    intervals :: Map (IntervalLit k) [a]
  }
  deriving (Eq, Show)

singleton :: (Interval k a) => a -> IntervalTree k a
singleton a =
  IntervalTree
    { index = Vector.singleton $ ofInterval a,
      intervals = Map.singleton (ofInterval a) [a]
    }

empty :: (Interval k a, Ord k) => IntervalTree k a
empty = IntervalTree mempty mempty

merge :: IntervalTree k a -> IntervalTree k a -> IntervalTree k a
merge = undefined

fromList :: (Show k, Show a, Ord k, Interval k a) => [a] -> IntervalTree k a
fromList [] = empty
fromList [x] = singleton x
fromList intervalData =
  -- index should be a (i_n, i_n+1) for each of the unique sorted starts + ends
  -- (if an interval starts/ends, the covered data changes)
  -- intervals should be:
  --   ... can I do a sliding window? can I do it if I covert to a vector?
  --   like of the interval at idx + takeBefore predicate + takeAfter predicate
  let allEndpoints = intervalData >>= (\i -> [intervalStart i, intervalEnd i])
      uniqueEndpoints = sort . nubOrd $ allEndpoints
      fullIndex = zipWith IntervalLit uniqueEndpoints (tail uniqueEndpoints)
      sortedIntervals = sortBy (\i1 i2 -> compare (intervalStart i1) (intervalStart i2)) intervalData
      -- for each interval in interval data
      -- drop from the index until the interval start is the key's beginning
      -- take until the key's end is the interval's end
      -- for each of those keys in the map (acc)
      -- add the piece of interval data
      mergeMaps = foldl' (Map.unionWithKey (const mappend)) mempty
      intervals =
        mergeMaps $
          sortedIntervals
            >>= ( \interval ->
                    let endpoints =
                          takeWhile (\key -> key <= intervalEnd interval) $
                            dropWhile (\key -> key < intervalStart interval) uniqueEndpoints
                        withTail = zip endpoints (tail endpoints)
                     in (\(s, e) -> Map.singleton (IntervalLit s e) [interval]) <$> withTail
                )
      index =
        Vector.fromList $
          filter
            (\idx -> not . null $ maybeToList (Map.lookup idx intervals))
            fullIndex
   in IntervalTree {index, intervals}

-- | Find the covering interval in sorted, non-overlapping interval lits.
findCoveringInterval :: (Show k, Ord k) => Vector.Vector (IntervalLit k) -> k -> Maybe (IntervalLit k)
findCoveringInterval xs k =
  case Vector.uncons xs of
    Nothing -> Nothing
    Just (h, t) | Vector.null t && h `contains` k -> Just h
    Just (h, t) | Vector.null t && not (h `contains` k) -> Nothing
    _ ->
      ( let (midpoint, _) = length xs `divMod` 2
            splitPoint = midpoint
            interval@(IntervalLit s e) = xs Vector.! splitPoint
            (left, right) = Vector.splitAt splitPoint xs
         in case (compare s k, compare e k) of
              (LT, GT) -> Just interval
              (EQ, _) -> Just interval
              (_, EQ) -> findCoveringInterval right k
              (_, LT) -> findCoveringInterval right k
              (GT, _) -> findCoveringInterval left k
      )

at :: (Interval k a, Show k) => IntervalTree k a -> k -> [a]
at (IntervalTree {index, intervals}) key =
  let coveringInterval = findCoveringInterval index key
      dataForInterval = coveringInterval >>= (`Map.lookup` intervals)
   in fold dataForInterval
