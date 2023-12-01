module AoC.Parser where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSChar
import Data.Functor (($>))
import qualified Data.Map as Map
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, Parsec, runParser, (<|>))
import Text.Megaparsec.Byte (string)

type Parser = Parsec Void ByteString

fromMap :: Map.Map String v -> Parser v
fromMap =
  Map.foldlWithKey'
    ( \parser k parsed ->
        parser <|> string (BSChar.pack k) $> parsed
    )
    (fail "empty map")

parsePuzzle :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
parsePuzzle parser = runParser parser ""
