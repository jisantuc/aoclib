module AoC.Data.Grid.Rectangular where

import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector

newtype RectangularGrid a = RectangularGrid {unGrid :: Vector (Vector a)} deriving (Eq, Show)

instance Functor RectangularGrid where
  fmap f (RectangularGrid mat) = RectangularGrid $ (fmap . fmap) f mat

instance Foldable RectangularGrid where
  foldMap f (RectangularGrid mat) =
    (foldMap . foldMap) f mat

type Point = (Int, Int)

data Direction a
  = Right a
  | Left a
  | Up a
  | Down a
  deriving (Eq, Ord, Show)

fromLists :: [[a]] -> RectangularGrid a
fromLists = RectangularGrid . Vector.fromList . (Vector.fromList <$>) . filter (not . null)

numRows :: RectangularGrid a -> Int
numRows (RectangularGrid mat) = Vector.length mat

numCols :: RectangularGrid a -> Int
numCols (RectangularGrid mat) = if Vector.null mat then 0 else Vector.length (mat ! 0)

shape :: RectangularGrid a -> (Int, Int)
shape grid = (numRows grid, numCols grid)

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
