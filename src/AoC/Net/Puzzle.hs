{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module AoC.Net.Puzzle
  ( fetch,
    Config (..),
  )
where

import AoC.Data.Puzzle (Day, Year)
import Control.Lens ((&), (.~), (^.))
import Control.Monad.Reader (MonadReader (ask), MonadTrans (lift), ReaderT)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text.Encoding (decodeASCII')
import qualified Data.Text.IO as TIO
import Network.Wreq (defaults, getWith, header)
import Network.Wreq.Lens (responseBody)
import System.Directory (doesFileExist)

data Config = Config
  { year :: Year,
    day :: Day,
    token :: Strict.ByteString,
    cacheFile :: FilePath
  }
  deriving (Eq, Show)

fetch :: ReaderT Config IO Text
fetch = do
  config@(Config {cacheFile}) <- ask
  fileExists <- lift $ doesFileExist cacheFile
  lift (if fileExists then TIO.readFile cacheFile else fetchFromUrl config)

fetchFromUrl :: Config -> IO Text
fetchFromUrl (Config {year, day, token, cacheFile}) =
  let url = "https://adventofcode.com/" <> show year <> "/day/" <> show day <> "/input"
      sessionCookie = "session=" <> token
      opts = defaults & header "Cookie" .~ [sessionCookie]
   in do
        resp <- getWith opts url <&> (^. responseBody)
        Lazy.writeFile cacheFile resp
        case decodeASCII' (Strict.toStrict resp) of
          Just t -> pure t
          Nothing -> error "No good ascii text"
