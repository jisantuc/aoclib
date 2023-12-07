module AoC.Optimization where

import Data.Foldable (maximumBy)

-- | Find the maximum input in `a` for values in `b`
-- This takes a function within the domain and an initial value
-- then recurses to find the value in the domain that corresponds to
-- the maximum output value
hillClimbWithStep :: (Ord a, Enum a, Integral a, Show a) => (a -> a) -> a -> a -> (a, a)
hillClimbWithStep f domainInitial = go f (domainInitial, f domainInitial)
  where
    go _ prevBest 0 = prevBest
    go f' prevBest stepSize' =
      let (init', _) = prevBest
          toLeft = init' - stepSize'
          toLeftValue = (toLeft, f' toLeft)
          toRight = init' + stepSize'
          toRightValue = (toRight, f' toRight)
          bestAvailable =
            maximumBy
              (\x y -> compare (snd x) (snd y))
              [toLeftValue, toRightValue, prevBest]
       in if bestAvailable == prevBest
            then go f' bestAvailable (stepSize' `div` 2)
            else go f' bestAvailable stepSize'
