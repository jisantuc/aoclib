module AoC.Data.Grid.Rectangular where

import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector

newtype RectangularGrid a = RectangularGrid (Vector (Vector a)) deriving (Eq, Show)

type Point = (Int, Int)

data Direction a
  = Right a
  | Left a
  | Up a
  | Down a
  deriving (Eq, Ord, Show)

fromLists :: [[a]] -> RectangularGrid a
fromLists = RectangularGrid . Vector.fromList . (Vector.fromList <$>) . filter (not . null)

replace :: RectangularGrid a -> Point -> a -> RectangularGrid a
replace (RectangularGrid mat) (row, col) v =
  RectangularGrid $ Vector.update mat $ Vector.singleton (row, Vector.update (mat ! row) (Vector.singleton (col, v)))

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

debugShow :: (a -> Char) -> RectangularGrid a -> String
debugShow p (RectangularGrid mat) =
  let results = Vector.toList $ Vector.toList . (p <$>) <$> mat
   in unlines results
