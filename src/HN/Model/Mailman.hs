-- | construct a feed from a mailman (pipermail) archive,
-- e.g., https://mail.haskell.org/pipermail/libraries/
-- see https://github.com/chrisdone/haskellnews/issues/12
-- and https://github.com/haskell-infra/hl/issues/63#issuecomment-74420850

{-# language OverloadedStrings #-}
{-# language BangPatterns #-}

module HN.Model.Mailman where

import Network.HTTP.Conduit
-- I tried with
-- import HN.Curl
-- but could not get it to work reliably:
-- segfaults, badcertfile, and other erratic behaviour
-- on opening the second HTTPS connection.

import Text.Feed.Types
import Text.Feed.Constructor

import qualified Text.HTML.DOM
import Text.XML
import Text.XML.Cursor
import qualified Data.Text as T 

test1 = do
  downloadFeed 10 "https://mail.haskell.org/pipermail/libraries/"


-- | look at archive and produce a feed.
-- 1st argument is (max.) number of items to produce.
-- 2st argument is "base name" of archive, e.g.
-- "https://mail.haskell.org/pipermail/libraries/"
-- it should end with "/"
downloadFeed :: Int -> String -> IO (Either String Feed)
downloadFeed its uri= do
  result <- getDoc uri
  case result of
    Left err -> return $ Left err
    Right doc -> do
      let cursor = fromDocument doc
      let entries = descendant cursor
                    >>= element "A"
                    >>= laxAttribute "href"
          dates = filter (T.isSuffixOf "date.html" ) entries 
      items <- getItems its uri $ take 1 dates
      -- mapM_ print items
      let feed0 = newFeed (RSSKind Nothing)
          feed = foldr addItem feed0 items
      print feed    
      return $ Right feed

getDoc uri = do
  -- hPutStrLn stderr $ unwords ["getDoc", uri]  
  result <- simpleHttp uri
  return $ Right $ Text.HTML.DOM.parseLBS result

getItems :: Int -> String -> [T.Text] -> IO [Item]
getItems its uri dates =
  if its <= 0 then return []
  else case dates of
    [] -> return []
    d:ates -> do
      result <- getDoc $ uri ++ T.unpack d
      let items = case result of
            Left err -> []
            Right doc ->
              let cursor = fromDocument doc
                  fetch desc = descendant cursor
                    >>= element "b"
                    >>= hasChildContents desc
                    >>= followingSibling
                    >>= element "i" >>= child >>= content
                  starting = fetch "Starting:"
                  ending   = fetch "Ending:"
              in  descendant cursor
                  >>= element "A"
                  >>= hasAttribute "HREF"
                  >>= parent
                  >>= element "LI"
                  >>= mkItem uri (starting,ending)
      later <- getItems (its - length items) uri ates
      return $ take its $ reverse items ++ later

hasChildContents desc =
  check $ \ c -> desc == T.concat (child c >>= content )
        
mkItem uri (starting,ending) c = do
  href <- child c >>= element "A" >>= laxAttribute "HREF"
  title <- child c >>= element "A" >>= hasAttribute "HREF"
           >>= child >>= content
  author <- child c >>= element "I" >>= child >>= content
  let unpack = T.unpack . T.unwords . T.words 
  return $ withItemLink (uri ++ T.unpack href)
         $ withItemTitle (unpack title)
         $ withItemAuthor (unpack author)
         $ case ending of
                [] -> id
                en:ding -> withItemPubDate (T.unpack en)
         $ newItem (RSSKind Nothing)
