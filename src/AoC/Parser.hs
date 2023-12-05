module AoC.Parser where

import Data.Functor (($>))
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, Parsec, runParser, (<|>))
import Text.Megaparsec.Byte (string)

type Parser = Parsec Void Text

fromMap :: Map.Map Text v -> Parser v
fromMap =
  Map.foldlWithKey'
    ( \parser k parsed ->
        parser <|> string k $> parsed
    )
    (fail "empty map")

parsePuzzle :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
parsePuzzle parser = runParser parser ""
