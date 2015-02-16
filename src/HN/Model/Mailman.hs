-- | construct a feed from a mailman (pipermail) archive,
-- e.g., https://mail.haskell.org/pipermail/libraries/
-- see https://github.com/chrisdone/haskellnews/issues/12
-- and https://github.com/haskell-infra/hl/issues/63#issuecomment-74420850

{-# language OverloadedStrings #-}

module HN.Model.Mailman where

import HN.Curl

-- import Text.Feed.Import
-- import Text.Feed.Query
import Text.Feed.Types

import qualified Text.HTML.DOM
import Text.XML
import Text.XML.Cursor

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C

import Network.URI

import System.IO

test1 = do
  downloadFeed "https://mail.haskell.org/pipermail/libraries/"

-- | look at archive and produce a feed.
downloadFeed :: String -> IO (Either String Feed)
downloadFeed uri = do
  result <- downloadString uri
  case result of
    Left e -> return (Left (show e))
{-
    Right str -> case parseFeedString str of
      Nothing -> do
        writeFile "/tmp/feed.xml" str
        return (Left ("Unable to parse feed from: " ++ uri))
      Just feed -> return (Right feed)
-}
    Right str -> do
      -- hPutStrLn stderr $ str
      let doc = Text.HTML.DOM.parseLBS $ C.pack str
      -- hPutStrLn stderr $ show doc
      let cursor = fromDocument doc
      let entries = descendant cursor >>= element "td"
                    >>= descendant >>= content
      mapM_ print $ entries 
      
      return $ Right  $ XMLFeed undefined
