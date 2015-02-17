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

import Data.Time.Format
import Data.Time.LocalTime ( ZonedTime )
import System.Locale ( rfc822DateFormat, defaultTimeLocale )

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
          archivedon = fetch_date cursor "Archived on:"
      items <- getItems its uri $ take 1 dates
      let pubdate = case archivedon of
            [] -> "Tue Feb 17 18:55:05 UTC 2015"
            d : _ -> d
      -- mapM_ print items
      let feed0 = newFeed (RSSKind Nothing)
          feed = withFeedPubDate pubdate
               $ foldr addItem feed0 items
      -- print feed    
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
                  starting = fetch_date cursor "Starting:"
                  ending   = fetch_date cursor "Ending:"
              in  descendant cursor
                  >>= element "A"
                  >>= hasAttribute "HREF"
                  >>= parent
                  >>= element "LI"
                  >>= mkItem uri (dropSuffix "date.html" d ) (starting,ending)
      later <- getItems (its - length items) uri ates
      return $ take its $ reverse items ++ later

dropSuffix suf s =
  if T.isSuffixOf suf s
  then T.take (T.length s - T.length suf) s
  else s

fetch_date cursor desc = do
  d <- fetch cursor desc
  -- we get this from mailman "Tue Feb 17 18:55:05 UTC 2015"
  -- and apparently we need "Tue, 17 Feb 2015 21:12:55 UTC"
  t <- maybe [] return $ parseTime defaultTimeLocale "%a %b %d %H:%M:%S %Z %Y" $ T.unpack d
  return $ formatTime defaultTimeLocale rfc822DateFormat ( t :: ZonedTime )

fetch cursor desc = descendant cursor
                    >>= element "b"
                    >>= hasChildContents desc
                    >>= followingSibling
                    >>= element "i" >>= child >>= content

hasChildContents desc =
  check $ \ c -> desc == T.concat (child c >>= content )
        
mkItem uri d (starting,ending) c = do
  href <- child c >>= element "A" >>= laxAttribute "HREF"
  title <- child c >>= element "A" >>= hasAttribute "HREF"
           >>= child >>= content
  author <- child c >>= element "I" >>= child >>= content
  let unpack = T.unpack . T.unwords . T.words
  return $ withItemLink (uri ++ T.unpack d ++ T.unpack href)
         $ withItemTitle (unpack title)
         $ withItemDescription (unpack title)
         $ withItemAuthor (unpack author)
         $ case ending of
                [] -> id
                -- FIXME: we put some date here,
                en:ding -> withItemPubDate en
         $ newItem (RSSKind Nothing)

