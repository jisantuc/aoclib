module AoC.Parser.CLI where

import AoC.Data.Puzzle (Day, Year)
import Options.Applicative (Parser, auto, long, metavar, option, short)

data PuzzleRunner = PuzzleRunner
  { year :: Year,
    day :: Day
  }
  deriving (Eq, Show)

runnerOptionsParser :: Parser PuzzleRunner
runnerOptionsParser =
  PuzzleRunner
    <$> option auto (long "year" <> short 'y' <> metavar "YEAR")
    <*> option auto (long "day" <> short 'd' <> metavar "DAY")
