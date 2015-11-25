-- | Output RSS feeds.

module Snap.App.RSS where

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           Snap.App
import           Snap.App.XML
import           Text.Feed.Export
import           Text.Feed.Types
import           Text.RSS.Syntax
import           Text.XML.Light

-- | Output the given XML element.
outputRSS :: String -> String -> [(UTCTime,Text,Text,Text)] -> Controller c s ()
outputRSS title link = outputXML . makeFeed title link

-- | Make a simple RSS feed.
makeFeed :: String -> String -> [(UTCTime,Text,Text,Text)] -> Element
makeFeed title link = xmlFeed . RSSFeed . makeRSS where
  makeRSS qs = (nullRSS title link)
               { rssChannel = makeChannel qs }
  makeChannel qs = (nullChannel title link)
                   { rssItems = map makeItem qs }
  makeItem (time,itemtitle,desc,itemlink) =
    (nullItem (T.unpack itemtitle))
    { rssItemPubDate = return (toPubDate time)
    , rssItemDescription = return (T.unpack desc)
    , rssItemLink = return (T.unpack itemlink)
    }
  toPubDate = formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S UT"
