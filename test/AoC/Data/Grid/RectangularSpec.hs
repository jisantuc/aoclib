{-# LANGUAGE LambdaCase #-}

module AoC.Data.Grid.RectangularSpec where

import AoC.Data.Grid.Rectangular
  ( RectangularGrid (..),
    debugShow,
    fromLists,
    manhattanDistance,
    rotateClockwise,
    rotateCounterClockwise,
    transpose,
  )
import qualified Data.Vector as Vector
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)

spec :: Spec
spec =
  describe "Rectangular grid" $ do
    it "constructs nicely from lists" $
      fromLists [['a', 'b'], ['c', 'd']]
        `shouldBe` RectangularGrid
          ( Vector.fromList [Vector.fromList ['a', 'b'], Vector.fromList ['c', 'd']]
          )
    describe "Manhattan distance" $ do
      it "calculates good distances for some example points" $ do
        manhattanDistance (0, 1) (-12, 28) `shouldBe` 39
        manhattanDistance (3, -4) (22, 5) `shouldBe` 19 + 9
      prop "calculates good distances from origin" $
        \squareParam flipY ->
          manhattanDistance (0, 0) (squareParam, if flipY then negate squareParam else squareParam)
            `shouldBe` 2 * abs squareParam
      prop "calculates the same distance for rotated points" $
        \p1@(x1, y1) p2@(x2, y2) ->
          manhattanDistance p1 p2 `shouldBe` manhattanDistance (y1, x1) (y2, x2)
    describe "transposition" $ do
      it "transposes an empty grid" $
        transpose (RectangularGrid mempty) `shouldBe` (RectangularGrid mempty :: RectangularGrid Int)
      it "transposes a single row" $
        transpose
          ( RectangularGrid
              ( Vector.fromList
                  [ Vector.fromList ['a'],
                    Vector.fromList ['b']
                  ]
              )
          )
          `shouldBe` RectangularGrid (pure $ Vector.fromList ['a', 'b'])
      it "transposes a 3x2 to a 2x3" $
        transpose
          ( RectangularGrid
              ( Vector.fromList
                  [ Vector.fromList ['a', 'b', 'c'],
                    Vector.fromList ['d', 'e', 'f']
                  ]
              )
          )
          `shouldBe` RectangularGrid
            ( Vector.fromList
                [ Vector.fromList ['a', 'd'],
                  Vector.fromList ['b', 'e'],
                  Vector.fromList ['c', 'f']
                ]
            )
    describe "rotation" $
      let exampleGrid =
            RectangularGrid
              ( Vector.fromList
                  [ Vector.fromList ['a', 'b', 'c'],
                    Vector.fromList ['d', 'e', 'f']
                  ]
              )
          twoByTwo =
            RectangularGrid
              ( Vector.fromList
                  [ Vector.fromList ['a', 'b'],
                    Vector.fromList ['c', 'd']
                  ]
              )
       in do
            it "has clockwise and counter clockwise rotations as inverses" $ do
              rotateClockwise (rotateCounterClockwise exampleGrid) `shouldBe` exampleGrid
              rotateCounterClockwise (rotateClockwise exampleGrid) `shouldBe` exampleGrid
            it "rotates 360 degrees as an identity" $ do
              (rotateClockwise . rotateClockwise . rotateClockwise . rotateClockwise) exampleGrid
                `shouldBe` exampleGrid
              (rotateCounterClockwise . rotateCounterClockwise . rotateCounterClockwise . rotateCounterClockwise)
                exampleGrid
                `shouldBe` exampleGrid
            it "rotates a simple 2x2 grid" $ do
              rotateClockwise twoByTwo
                `shouldBe` RectangularGrid
                  ( Vector.fromList
                      [ Vector.fromList ['c', 'a'],
                        Vector.fromList ['d', 'b']
                      ]
                  )
              (rotateClockwise . rotateClockwise) twoByTwo
                `shouldBe` RectangularGrid
                  ( Vector.fromList
                      [ Vector.fromList ['d', 'c'],
                        Vector.fromList ['b', 'a']
                      ]
                  )
              (rotateClockwise . rotateClockwise . rotateClockwise) twoByTwo
                `shouldBe` RectangularGrid
                  ( Vector.fromList
                      [ Vector.fromList ['b', 'd'],
                        Vector.fromList ['a', 'c']
                      ]
                  )
              (rotateClockwise . rotateClockwise) twoByTwo
                `shouldBe` (rotateCounterClockwise . rotateCounterClockwise) twoByTwo
              (rotateClockwise . rotateClockwise . rotateClockwise) twoByTwo
                `shouldBe` rotateCounterClockwise twoByTwo
    describe "debugging" $
      it "prints a nice string" $ do
        debugShow
          ( \case
              'a' -> '.'
              _ -> '#'
          )
          (fromLists [['a', 'b'], ['c', 'd']])
          `shouldBe` ".#\n##\n"
