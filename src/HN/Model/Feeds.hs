-- | Download and import feeds from various sources.

module HN.Model.Feeds where

import HN.Data
import HN.Monads
import HN.Model.Items
import HN.Types
import HN.Curl

import Control.Applicative
import Network.URI
import Snap.App

import Text.Feed.Import
import Text.Feed.Query
import Text.Feed.Types

--------------------------------------------------------------------------------
-- Various service feeds

importHaskellCafe = do
  importGenerically HaskellCafe
                    "https://groups.google.com/forum/feed/haskell-cafe/msgs/rss_v2_0.xml"
                    (\item -> return (item { niTitle = strip (niTitle item) }))

  where strip x | isPrefixOf "re: " (map toLower x) = strip (drop 4 x)
                | isPrefixOf label x = drop (length label) x
                | otherwise = x
        label = "[Haskell-cafe]"

importPlanetHaskell =
  importGeneric PlanetHaskell "http://planet.haskell.org/rss20.xml"

importJobs =
  importGeneric Jobs "http://www.haskellers.com/feed/jobs"

importStackOverflow = do
  importGeneric StackOverflow "http://stackoverflow.com/feeds/tag/haskell"
  importGeneric StackOverflow "http://programmers.stackexchange.com/feeds/tag/haskell"

importHaskellWiki =
  importGeneric HaskellWiki "http://wiki.haskell.org/index.php?title=Special:RecentChanges&feed=atom"

importHackage =
  importGeneric Hackage "http://hackage.haskell.org/packages/recent.rss"
-- Old feed is gone:
-- importGeneric Hackage "http://hackage.haskell.org/recent.rss"
-- Old feed is gone:
-- importGeneric Hackage "http://hackage.haskell.org/packages/archive/recent.rss"

-- | Import all vimeo content.
importVimeo = do
  importGeneric Vimeo "https://vimeo.com/channels/haskell/videos/rss"
  importGeneric Vimeo "https://vimeo.com/channels/galois/videos/rss"
  importGenerically Vimeo "https://vimeo.com/rickasaurus/videos/rss"
    (\ni -> if isInfixOf "haskell" (map toLower (niTitle ni)) then return ni else Nothing)

-- | Import @remember'd IRC quotes from ircbrowse.
importIrcQuotes = do
  importGeneric IrcQuotes
                "http://ircbrowse.net/quotes.rss"

-- | Import pastes about Haskell.
importPastes = do
  importGeneric Pastes
                "http://lpaste.net/channel/haskell/rss"

--------------------------------------------------------------------------------
-- Reddit

-- | Get /r/haskell.
importRedditHaskell = do
  result <- io $ getReddit "haskell"
  case result of
    Left e -> return (Left e)
    Right items -> do
      mapM_ (addItem Reddit) items
      return (Right ())

-- | Import from proggit.
importProggit = do
  result <- io $ getReddit "programming"
  case result of
    Left e -> return (Left e)
    Right items -> do
      mapM_ (addItem Reddit) (filter (hasHaskell . niTitle) items)
      return (Right ())

  where hasHaskell = isInfixOf "haskell" . map toLower

-- | Get Reddit feed.
getReddit subreddit = do
  result <- downloadFeed ("https://www.reddit.com/r/" ++ subreddit ++ "/.rss")
  case result of
    Left e -> return (Left e)
    Right e -> return (mapM makeItem (feedItems e))

--------------------------------------------------------------------------------
-- Get feeds

-- | Import from a generic feed source.
importGeneric :: Source -> String -> Model c s (Either String ())
importGeneric source uri = do
  importGenerically source uri return

-- | Import from a generic feed source.
importGenerically :: Source -> String -> (NewItem -> Maybe NewItem) -> Model c s (Either String ())
importGenerically source uri f = do
  result <- io $ downloadFeed uri
  case result >>= mapM (fmap f . makeItem) . feedItems of
    Left e -> do
      return (Left e)
    Right items -> do
      mapM_ (addItem source) (catMaybes items)
      return (Right ())

-- | Make an item from a feed item.
makeItem :: Item -> Either String NewItem
makeItem item =
  NewItem <$> extract "item" (getItemTitle item)
          <*> extract "publish date" (join (getItemPublishDate item))
          <*> extract "description" (getItemDescription item <|> getItemTitle item)
          <*> extract "link" (getItemLink item >>= parseURILeniently)

  where extract label = maybe (Left ("Unable to extract " ++ label)) Right

-- | Escape any characters not allowed in URIs because at least one
-- feed (I'm looking at you, reddit) do not escape characters like ö.
parseURILeniently :: String -> Maybe URI
parseURILeniently = parseURI . escapeURIString isAllowedInURI

-- | Download and parse a feed.
downloadFeed :: String -> IO (Either String Feed)
downloadFeed uri = do
  result <- downloadString uri
  case result of
    Left e -> return (Left (show e))
    Right str -> case parseFeedString str of
      Nothing -> do
        writeFile "/tmp/feed.xml" str
        return (Left ("Unable to parse feed from: " ++ uri))
      Just feed -> return (Right feed)

--------------------------------------------------------------------------------
-- Utilities

-- | Parse one of the two dates that might occur out there.
parseDate x = parseRFC822 x <|> parseRFC3339 x

-- | Parse an RFC 3339 timestamp.
parseRFC3339 :: String -> Maybe ZonedTime
parseRFC3339 = parseTime defaultTimeLocale "%Y-%m-%dT%TZ"

-- | Parse an RFC 822 timestamp.
parseRFC822 :: String -> Maybe ZonedTime
parseRFC822 = parseTime defaultTimeLocale rfc822DateFormat
