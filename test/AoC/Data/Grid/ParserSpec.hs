module AoC.Data.Grid.ParserSpec where

import AoC.Data.Grid.Parser (sourcePositionToPoint)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (SourcePos (..), mkPos)

spec :: Spec
spec = describe "ParserSpec" $ do
  it "0-indexes rows and columns" $
    sourcePositionToPoint
      ( SourcePos
          { sourceLine = mkPos 1,
            sourceColumn = mkPos 1,
            sourceName = "bogus"
          }
      )
      `shouldBe` (0, 0)
