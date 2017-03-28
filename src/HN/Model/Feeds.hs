-- | Download and import feeds from various sources.

module HN.Model.Feeds where

import HN.Data
import HN.Monads
import HN.Model.Items
import HN.Types
import HN.Curl

import qualified HN.Model.Mailman (downloadFeed)

import Control.Applicative
import Network.URI
import Snap.App

import Text.Feed.Import
import Text.Feed.Query
import Text.Feed.Types

--------------------------------------------------------------------------------
-- Various service feeds

importHaskellCafe :: Model c s (Either String ())
importHaskellCafe =
  importMailman 50
                HaskellCafe
                "https://mail.haskell.org/pipermail/haskell-cafe/"
                (\item -> return (item { niTitle = strip "[Haskell-cafe]" (niTitle item) }))

importLibraries :: Model c s (Either String ())
importLibraries =
  importMailman 50
                Libraries
                "https://mail.haskell.org/pipermail/libraries/"
                (\item -> return (item { niTitle = strip "[libraries]" (niTitle item) }))

importGhcDevs :: Model c s (Either String ())
importGhcDevs =
  importMailman 50
                GhcDevs
                "https://mail.haskell.org/pipermail/ghc-devs/"
                (\item -> return (item { niTitle = strip "[ghc-devs]" (niTitle item) }))


strip label x | isPrefixOf "re: " (map toLower x) = strip label (drop 4 x)
                | isPrefixOf label x = drop (length label) x
                | otherwise = x

importPlanetHaskell :: Model c s (Either String ())
importPlanetHaskell =
  importGeneric PlanetHaskell "http://planet.haskell.org/rss20.xml"

importJobs :: Model c s (Either String ())
importJobs =
  importGeneric Jobs "http://www.haskellers.com/feed/jobs"

importStackOverflow :: Model c s (Either String ())
importStackOverflow = do
  importGeneric StackOverflow "http://stackoverflow.com/feeds/tag/haskell"
  importGeneric StackOverflow "http://programmers.stackexchange.com/feeds/tag/haskell"
  importGeneric StackOverflow "http://codereview.stackexchange.com/feeds/tag/haskell"

importHaskellWiki :: Model c s (Either String ())
importHaskellWiki =
  importGeneric HaskellWiki "http://wiki.haskell.org/index.php?title=Special:RecentChanges&feed=atom"

importHackage :: Model c s (Either String ())
importHackage =
  importGeneric Hackage "http://hackage.haskell.org/packages/recent.rss"

-- | Import all vimeo content.
importVimeo :: Model c s (Either String ())
importVimeo = do
  importGeneric Vimeo "https://vimeo.com/channels/haskell/videos/rss"
  importGeneric Vimeo "https://vimeo.com/channels/galois/videos/rss"
  importGenerically Vimeo "https://vimeo.com/rickasaurus/videos/rss"
    (\ni -> if isInfixOf "haskell" (map toLower (niTitle ni)) then return ni else Nothing)

-- | Import @remember'd IRC quotes from ircbrowse.
importIrcQuotes :: Model c s (Either String ())
importIrcQuotes = do
  importGeneric IrcQuotes
                "http://ircbrowse.net/quotes.rss"

-- | Import pastes about Haskell.
importPastes :: Model c s (Either String ())
importPastes = do
  importGeneric Pastes
                "http://lpaste.net/channel/haskell/rss"

--------------------------------------------------------------------------------
-- Reddit

-- | Get /r/haskell.
importRedditHaskell :: Model c s (Either String ())
importRedditHaskell = do
  result <- io $ getReddit "haskell"
  case result of
    Left e -> return (Left e)
    Right items -> do
      mapM_ (addItem Reddit) items
      return (Right ())

-- | Import from proggit.
importProggit :: Model c s (Either String ())
importProggit = do
  result <- io $ getReddit "programming"
  case result of
    Left e -> return (Left e)
    Right items -> do
      mapM_ (addItem Reddit) (filter (hasHaskell . niTitle) items)
      return (Right ())

  where hasHaskell = isInfixOf "haskell" . map toLower

-- | Get Reddit feed.
getReddit :: String -> IO (Either String [NewItem])
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

importMailman :: Int -> Source -> String -> (NewItem -> Maybe NewItem) -> Model c s (Either String ())
importMailman its source uri f = do
  result <- io $ HN.Model.Mailman.downloadFeed its uri
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

  where extract label = maybe (Left ("Unable to extract " ++ label ++ " for " ++ show item)) Right

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
parseDate :: String -> Maybe ZonedTime
parseDate x = parseRFC822 x <|> parseRFC3339 x

-- | Parse an RFC 3339 timestamp.
parseRFC3339 :: String -> Maybe ZonedTime
parseRFC3339 = (parseTimeM True) defaultTimeLocale "%Y-%m-%dT%TZ"

-- | Parse an RFC 822 timestamp.
parseRFC822 :: String -> Maybe ZonedTime
parseRFC822 = (parseTimeM True) defaultTimeLocale rfc822DateFormat
