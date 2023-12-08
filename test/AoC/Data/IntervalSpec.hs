module AoC.Data.IntervalSpec where

import AoC.Data.Interval (Interval (..), covers, touches)
import qualified AoC.Data.Interval as Interval
import Data.Foldable (traverse_)
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)

spec :: Spec
spec = describe "intervals" $ do
  describe "point lookup" $ do
    it "finds points in ranges correctly" $ do
      (4 :: Int) `Interval.elem` Interval 4 6 `shouldBe` True
      (6 :: Int) `Interval.elem` Interval 4 6 `shouldBe` True
      (7 :: Int) `Interval.elem` Interval 4 6 `shouldNotBe` True
  describe "range lookup" $
    let interval1 :: Interval Int
        interval1 = Interval 5 10
        interval2 :: Interval Int
        interval2 = Interval 7 12
        interval3 :: Interval Int
        interval3 = Interval 10 15
        interval4 :: Interval Int
        interval4 = Interval 0 15
        interval5 :: Interval Int
        interval5 = Interval 100 101
     in do
          it "finds whether ranges touch correctly" $
            do
              traverse_
                (\interval -> interval `touches` interval `shouldBe` True)
                [interval1, interval2, interval3, interval4, interval5]
              traverse_
                (\interval -> interval1 `touches` interval `shouldBe` True)
                [interval2, interval3, interval4]
              interval1 `touches` interval5 `shouldBe` False
          it "finds whether ranges cover correctly" $
            do
              traverse_
                (\interval -> interval `covers` interval `shouldBe` True)
                [interval1, interval2, interval3, interval4, interval5]
              traverse_
                (\interval -> interval4 `covers` interval `shouldBe` True)
                [interval1, interval2, interval3]
              traverse_
                (\interval -> interval5 `covers` interval `shouldBe` False)
                [interval1, interval2, interval3, interval4]
