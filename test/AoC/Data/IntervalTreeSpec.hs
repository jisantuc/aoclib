module AoC.Data.IntervalTreeSpec where

import AoC.Data.Interval (IntervalLit (..))
import AoC.Data.IntervalTree
  ( IntervalTree (..),
    at,
    findCoveringInterval,
  )
import qualified AoC.Data.IntervalTree as IntervalTree
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vector
import Test.Hspec (Expectation, Spec, describe, it, shouldBe, xit)

-- TODO:
-- don't test construction,  `atInterval`, `insert`, `merge`, and `delete`
spec :: Spec
spec = describe "interval trees" $ do
  describe "construction" $ do
    it "builds from a list singleton" $
      IntervalTree.fromList [IntervalLit 'a' 'f']
        `shouldBe` IntervalTree
          { intervals = Map.singleton (IntervalLit 'a' 'f') [IntervalLit 'a' 'f'],
            index = Vector.fromList [IntervalLit 'a' 'f']
          }
    it "builds from a two-element list of non-overlapping intervals" $
      let firstInterval = IntervalLit 'a' 'f'
          secondInterval = IntervalLit 'h' 'j'
       in IntervalTree.fromList [firstInterval, secondInterval]
            `shouldBe` IntervalTree
              { index = Vector.fromList [firstInterval, secondInterval],
                intervals = Map.fromList [(firstInterval, [firstInterval]), (secondInterval, [secondInterval])]
              }
    it "builds from a two-element list of overlapping intervals" $
      let firstInterval = IntervalLit 'a' 'f'
          secondInterval = IntervalLit 'e' 'k'
       in IntervalTree.fromList [firstInterval, secondInterval]
            `shouldBe` IntervalTree
              { index = Vector.fromList [IntervalLit 'a' 'e', IntervalLit 'e' 'f', IntervalLit 'f' 'k'],
                intervals =
                  Map.fromList
                    [ (IntervalLit 'a' 'e', [firstInterval]),
                      (IntervalLit 'e' 'f', [firstInterval, secondInterval]),
                      (IntervalLit 'f' 'k', [secondInterval])
                    ]
              }
  describe "querying at a point" $
    let oneElementIntervalTree = IntervalTree.fromList [IntervalLit 'a' 'f']
        twoElementIntervalTree = IntervalTree.fromList [IntervalLit 'a' 'f', IntervalLit 'h' 'k']
        overlappingRangesIntervalTree = IntervalTree.fromList [IntervalLit 'a' 'j', IntervalLit 'b' 'k']
     in do
          it "finds intervals at points" $ do
            oneElementIntervalTree `at` 'b' `shouldBe` [IntervalLit 'a' 'f']
            twoElementIntervalTree `at` 'a' `shouldBe` [IntervalLit 'a' 'f']
            twoElementIntervalTree `at` 'h' `shouldBe` [IntervalLit 'h' 'k']
          it "finds all intervals at points" $ do
            overlappingRangesIntervalTree `at` 'a' `shouldBe` [IntervalLit 'a' 'j']
            overlappingRangesIntervalTree `at` 'b' `shouldBe` [IntervalLit 'a' 'j', IntervalLit 'b' 'k']
            overlappingRangesIntervalTree `at` 'i' `shouldBe` [IntervalLit 'a' 'j', IntervalLit 'b' 'k']
            overlappingRangesIntervalTree `at` 'j' `shouldBe` [IntervalLit 'b' 'k']
          it "doesn't find intervals at points that aren't included" $ do
            oneElementIntervalTree `at` 'f' `shouldBe` []
            oneElementIntervalTree `at` 'g' `shouldBe` []
            twoElementIntervalTree `at` 'g' `shouldBe` []
            overlappingRangesIntervalTree `at` 'k' `shouldBe` []
  describe "querying at an interval" $ do
    xit "finds intervals over ranges" pass
  describe "finding the right key" $ do
    it "doesn't find a key when not present or in an empty list" $ do
      findCoveringInterval (Vector.fromList []) 'a' `shouldBe` Nothing
      findCoveringInterval (Vector.fromList [IntervalLit 'b' 'e']) 'a' `shouldBe` Nothing
      findCoveringInterval (Vector.fromList [IntervalLit 'b' 'e']) 'e' `shouldBe` Nothing
      findCoveringInterval (Vector.fromList [IntervalLit 'b' 'e']) 'b' `shouldBe` Just (IntervalLit 'b' 'e')
      findCoveringInterval (Vector.fromList [IntervalLit 'b' 'e', IntervalLit 'e' 'k', IntervalLit 'm' 'p']) 'l'
        `shouldBe` Nothing
      findCoveringInterval
        ( Vector.fromList
            [ IntervalLit 'b' 'e',
              IntervalLit 'e' 'k',
              IntervalLit 'm' 'p',
              IntervalLit 'p' 'q',
              IntervalLit 'r' 't'
            ]
        )
        'l'
        `shouldBe` Nothing
    it "finds a key when present in any size list" $ do
      findCoveringInterval (Vector.fromList [IntervalLit 'b' 'e']) 'c' `shouldBe` Just (IntervalLit 'b' 'e')
      findCoveringInterval (Vector.fromList [IntervalLit 'a' 'd', IntervalLit 'f' 'g', IntervalLit 'g' 'i']) 'c'
        `shouldBe` Just (IntervalLit 'a' 'd')
      findCoveringInterval (Vector.fromList [IntervalLit 'a' 'd', IntervalLit 'f' 'g', IntervalLit 'g' 'i']) 'h'
        `shouldBe` Just (IntervalLit 'g' 'i')
      findCoveringInterval (Vector.fromList ((\c -> IntervalLit (pred c) c) <$> ['b' .. 'z'])) 'm'
        `shouldBe` Just (IntervalLit 'm' 'n')

pass :: Expectation
pass = () `shouldBe` ()
