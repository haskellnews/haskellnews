-- | construct a feed from a mailman (pipermail) archive,
-- e.g., https://mail.haskell.org/pipermail/libraries/
-- see https://github.com/chrisdone/haskellnews/issues/12
-- and https://github.com/haskell-infra/hl/issues/63#issuecomment-74420850

{-# language OverloadedStrings #-}

module HN.Model.Mailman where

import Network.HTTP.Conduit

import Text.Feed.Types
import Text.Feed.Query (getItemTitle)
import Text.Feed.Constructor

import qualified Text.HTML.DOM
import Text.XML
import Text.XML.Cursor as XMLCursor
import Text.XML.Cursor.Generic as XMLGeneric
import qualified Data.Text as T

import Data.Time.Format
import Data.Time.LocalTime ( ZonedTime )
import Data.Char (isDigit)

import qualified Data.Set as S

test1 :: IO (Either String Feed)
test1 = downloadFeed 10 "https://mail.haskell.org/pipermail/haskell-cafe/"


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
          archivedon = fetchDate cursor "Archived on:"
      items <- getItems its uri $ take 1 dates
      let pubdate = case archivedon of
            [] -> "Tue Feb 17 18:55:05 UTC 2015"
            d : _ -> d
      let feed0 = newFeed (RSSKind Nothing)
          feed = withFeedPubDate pubdate
              $ foldr addItem feed0 items
      return $ Right feed

getDoc :: String -> IO (Either a Document)
getDoc uri = do
  result <- simpleHttp uri
  return $ Right $ Text.HTML.DOM.parseLBS result

-- | get at least @its@ items (since we cannot easily sort by time here,
-- we get the full month, and if it contains less that @its@ messages,
-- we get the previous month as well, etc.)
getItems :: Int -> String -> [T.Text] -> IO [Item]
getItems its uri dates = getItemsUnique S.empty its uri dates

-- | get only the newest item for each subject
getItemsUnique :: S.Set (Maybe String) -> Int -> String -> [T.Text] -> IO [Item]
getItemsUnique seen its uri dates =
  if its <= 0 then return []
  else case dates of
    [] -> return []
    d:ates -> do
      result <- getDoc $ uri ++ T.unpack d
      let (seen', items) = uniques seen $ reverse $ case result of
            Left _ -> []
            Right doc ->
              let cursor = fromDocument doc
              in  descendant cursor
                  >>= element "A"
                  >>= hasAttribute "HREF"
                  >>= parent
                  >>= element "LI"
                  >>= mkItem uri (dropSuffix "thread.html" d )
      later <- getItemsUnique seen' (its - length items) uri ates
      return $ items ++ later

uniques seen [] = (seen, [])
uniques seen (x:xs) = 
  let s = getItemTitle x
  in  if S.member s seen 
      then uniques seen xs
      else let (seen', xs') = uniques (S.insert s seen) xs
           in  (seen', x:xs')

dropSuffix :: T.Text -> T.Text -> T.Text
dropSuffix suf s =
  if T.isSuffixOf suf s
  then T.take (T.length s - T.length suf) s
  else s

fetchDate :: XMLGeneric.Cursor Node -> T.Text -> [String]
fetchDate cursor desc = do
  d <- fetch cursor desc
  -- we get this from mailman "Tue Feb 17 18:55:05 UTC 2015"
  -- and apparently we need "Tue, 17 Feb 2015 21:12:55 UTC"
  t <- maybe [] return $ parseTimeM True defaultTimeLocale "%a %b %d %H:%M:%S %Z %Y" $ T.unpack d
  return $ formatTime defaultTimeLocale rfc822DateFormat ( t :: ZonedTime )

fetch :: XMLGeneric.Cursor Node -> T.Text -> [T.Text]
fetch cursor desc = descendant cursor
                    >>= element "b"
                    >>= hasChildContents desc
                    >>= followingSibling
                    >>= element "i" >>= child >>= content

hasChildContents :: T.Text -> XMLCursor.Axis
hasChildContents desc = check $ \ c -> desc == T.concat (child c >>= content )

mkItem :: String -> T.Text -> XMLGeneric.Cursor Node -> [Item]
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
  let (s,_) = T.breakOnEnd "." com
      n = T.reverse $ T.takeWhile isDigit $ T.drop 1 $ T.reverse s
      Just pubdate = parseTimeM True defaultTimeLocale "%s" $ T.unpack n
      pubdateString = formatTime defaultTimeLocale rfc822DateFormat ( pubdate :: ZonedTime )
  let unpack = T.unpack . T.unwords . T.words
  return $ withItemLink (uri ++ T.unpack d ++ T.unpack href)
         $ withItemTitle (unpack title)
         $ withItemDescription (unpack title)
         $ withItemAuthor (unpack author)
         $ withItemPubDate pubdateString
         $ newItem (RSSKind Nothing)

