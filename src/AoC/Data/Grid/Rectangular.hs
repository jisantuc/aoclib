module AoC.Data.Grid.Rectangular where

import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector

newtype RectangularGrid a = RectangularGrid (Vector (Vector a)) deriving (Eq, Show)

type Point = (Int, Int)

data Direction
  = Right Int
  | Left Int
  | Up Int
  | Down Int

manhattanDistance :: Point -> Point -> Int
manhattanDistance (x1, y1) (x2, y2) =
  abs (x1 - x2) + abs (y1 - y2)

transpose :: RectangularGrid a -> RectangularGrid a
transpose g@(RectangularGrid v) =
  if null v
    then g
    else
      let nCols = Vector.length (v ! 0)
          columnIndices = Vector.fromList [0 .. nCols - 1]
       in RectangularGrid $ (\j -> (! j) <$> v) <$> columnIndices

rotateClockwise :: RectangularGrid a -> RectangularGrid a
rotateClockwise grid =
  let (RectangularGrid transposedMat) = transpose grid
   in RectangularGrid $ Vector.reverse <$> transposedMat

rotateCounterClockwise :: RectangularGrid a -> RectangularGrid a
rotateCounterClockwise (RectangularGrid grid) =
  transpose $
    RectangularGrid (Vector.reverse <$> grid)
