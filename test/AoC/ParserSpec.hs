{-# LANGUAGE OverloadedStrings #-}

module AoC.ParserSpec where

import qualified AoC.Parser as Parser
import Data.Either (isLeft)
import qualified Data.Map as Map
import Data.Text (Text)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (runParser)

spec :: Spec
spec = describe "Parser" $ do
  describe "fromMap" $ do
    it "should build a parser that fails for any input from an empty map" $
      isLeft (runParser (Parser.fromMap mempty) "" "abcde") `shouldBe` True
    it "should build a parser that succeeds on the correct input for a single-element map" $
      let parserMap :: Map.Map Text Int
          parserMap = Map.fromList [("abc", 1)]
       in runParser (Parser.fromMap parserMap) "" "abc" `shouldBe` Right 1
    it "should build a parser that succeeds on the correct inputs for a two-element map" $
      let parserMap :: Map.Map Text Int
          parserMap = Map.fromList [("abc", 1), ("def", 2)]
       in do
            runParser (Parser.fromMap parserMap) "" "abc" `shouldBe` Right 1
            runParser (Parser.fromMap parserMap) "" "def" `shouldBe` Right 2
            isLeft (runParser (Parser.fromMap parserMap) "" "bogus") `shouldBe` True
