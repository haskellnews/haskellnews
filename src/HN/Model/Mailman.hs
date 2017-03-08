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
-- import Data.Time ( rfc822DateFormat, defaultTimeLocale )
import Data.Char (isDigit)

test1 = do
  downloadFeed 10 "https://mail.haskell.org/pipermail/haskell-cafe/"


-- | look at archive and produce a feed.
-- 1st argument is minimal number of items to produce.
-- 2st argument is "base name" of archive, e.g.
-- "https://mail.haskell.org/pipermail/haskell-cafe/"
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
          dates = filter (T.isSuffixOf "thread.html" ) entries
          archivedon = fetch_date cursor "Archived on:"
      items <- getItems its uri $ take 1 dates
      let pubdate = case archivedon of
            [] -> "Tue Feb 17 18:55:05 UTC 2015"
            d : _ -> d
      let feed0 = newFeed (RSSKind Nothing)
          feed = id
               $ withFeedPubDate pubdate
               $ foldr addItem feed0 items
      return $ Right feed

getDoc uri = do
  result <- simpleHttp uri
  return $ Right $ Text.HTML.DOM.parseLBS result

-- | get at least @its@ items (since we cannot easily sort by time here,
-- we get the full month, and if it contains less that @its@ messages,
-- we get the previous month as well, etc.)
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
              in  descendant cursor
                  >>= element "A"
                  >>= hasAttribute "HREF"
                  >>= parent
                  >>= element "LI"
                  >>= mkItem uri (dropSuffix "thread.html" d ) 
      later <- getItems (its - length items) uri ates
      return $ items ++ later

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
        
mkItem uri d c = do
  href <- child c >>= element "A" >>= laxAttribute "HREF"
  title <- child c >>= element "A" >>= hasAttribute "HREF"
           >>= child >>= content
  author <- child c >>= element "I" >>= child >>= content

  -- UGLY date hack. The information looks like this:
  -- NodeComment "2 01424167545.118214-01424175253.118215-01424184970.118219- "
  -- and we need to get to this part: --------------------^^^^^^^^^^^
  -- which gives the epoch time of the current message
  NodeComment com <- map node ( precedingSibling c >>= precedingSibling )
  let (s,t) = T.breakOnEnd "." com
      n = T.reverse $ T.takeWhile isDigit $ T.drop 1 $ T.reverse s
      Just pubdate = parseTime defaultTimeLocale "%s" $ T.unpack n
      pubdateString = formatTime defaultTimeLocale rfc822DateFormat ( pubdate :: ZonedTime )
  let unpack = T.unpack . T.unwords . T.words
  return $ withItemLink (uri ++ T.unpack d ++ T.unpack href)
         $ withItemTitle (unpack title)
         $ withItemDescription (unpack title)
         $ withItemAuthor (unpack author)
         $ withItemPubDate pubdateString
         $ newItem (RSSKind Nothing)

