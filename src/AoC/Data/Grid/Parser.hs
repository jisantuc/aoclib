{-# LANGUAGE NamedFieldPuns #-}

module AoC.Data.Grid.Parser where

import AoC.Data.Grid.Rectangular (Point)
import Text.Megaparsec (SourcePos (..), unPos)

sourcePositionToPoint :: SourcePos -> Point
sourcePositionToPoint (SourcePos {sourceLine, sourceColumn}) = (unPos sourceLine - 1, unPos sourceColumn - 1)
