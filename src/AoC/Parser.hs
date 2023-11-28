module AoC.Parser where

import Data.ByteString (ByteString)
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, Parsec, runParser)

type Parser = Parsec Void ByteString

parsePuzzle :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
parsePuzzle parser = runParser parser ""
