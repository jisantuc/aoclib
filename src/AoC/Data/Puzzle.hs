{-# LANGUAGE DerivingVia #-}

module AoC.Data.Puzzle where

newtype Year = Year Int deriving (Eq, Read, Show) via Int

newtype Day = Day Int deriving (Eq, Read, Show) via Int
