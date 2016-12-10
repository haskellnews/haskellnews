{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Download and import from HTML soup because the services were too
-- stingy to provide an RSS feed.

module HN.Model.Soup where

import HN.Data
import HN.Monads
import HN.Model
import HN.Model.Items
import HN.Types
import HN.Curl

import Control.Applicative
import Control.Monad.Error
import Data.Time
import Network.URI
import Snap.App

import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import TagSoup

--------------------------------------------------------------------------------
-- Google+

importGooglePlus :: Model c s (Either String ())
importGooglePlus = do
  result <- io $ downloadString "https://plus.google.com/communities/104818126031270146189"
  case result of
    Left e -> return (Left (show e))
    Right html ->
      case runSoup (parseTags html) getShares of
        Left e -> return (Left e)
        Right items -> do mapM_ (addItem GooglePlus) items
                          return (Right ())

-- | Get Google+ shares.
getShares :: Soup [NewItem]
getShares = do
  skipTagByName "header"
  skipTagByName "a"
  author <- nextText
  a <- gotoTagByName "a"
  link <- fmap ("https://plus.google.com/" ++) (getAttrib "href" a)
  uri <- meither "couldn't parse URI" (parseURI link)
  tstr <- nextText
  time <- parseDate tstr
  skipTagByName "span"
  skipTagByName "div"
  skipTagByName "div"
  skipTagByName "div"
  desc <- get >>= return . tagsText . takeWhile (not . tagOpen (const True) (any (isInfixOf "+1") . map snd))
  items <- getShares <|> return []
  return $ NewItem
    { niTitle = trim author ++ " shares: " ++ trim (unwords (take 10 (words desc)) ++ " ...")
    , niDescription = desc
    , niLink = uri
    , niPublished = time
    } : items

--------------------------------------------------------------------------------
-- Twitter

-- | Import all "haskell" mentions on twitter.
importHaskellTwitter :: Model c s (Either String ())
importHaskellTwitter =
  importFromTwitter
    "https://twitter.com/search?q=haskell%20-rugby%20-jewelry%20%23haskell&src=typd"

-- | Import mentions of @haskelltips.
importHaskellTips :: Model c s (Either String ())
importHaskellTips =
  importFromTwitter
    "https://twitter.com/HaskellTips"

-- | Import recent Tweets from the search.
importFromTwitter :: String -> Model c s (Either String ())
importFromTwitter url = do
  result <- io $ downloadString url
  case result of
    Left e -> return (Left (show e))
    Right str ->
      case runSoup (parseTags str) extractTwitterItems of
        Left e -> return (Left e)
        Right items -> do mapM_ (addItem Twitter) items
                          return (Right ())

-- | Skip to each tweet and extract the items.
extractTwitterItems :: Soup [NewItem]
extractTwitterItems = go where
  go = do
    skipTagByNameClass "li" "stream-item"
    skipTagByNameClass "div" "original-tweet"
    skipTagByNameClass "div" "content"
    skipTagByNameClass "span" "username"
    skipTagByName "b"
    username <- nextText
    a <- gotoTagByNameClass "a" "tweet-timestamp"
    link <- getAttrib "href" a
    uri <- meither "couldn't parse URI" (parseURI ("https://twitter.com" ++ link))
    timestamp <- gotoTagByName "span"
    epoch <- getAttrib "data-time" timestamp
    published <- parseEpoch epoch
    gotoTagByNameClass "p" "tweet-text"
    tags <- get
    let tweet = tagsTxt (takeWhile (not . tagCloseLit "p") tags)
    items <- go <|> return []
    return $ NewItem
      { niTitle = username ++ ": " ++ tweet
      , niPublished = published
      , niDescription = ""
      , niLink = uri
      } : items

--------------------------------------------------------------------------------
-- Events from HaskellWiki

-- | Import events from the HaskellWiki template. I'm not sure how
-- reliable this feed is in general, but it's better than zero event
-- information.

-- | Import events.
importEvents :: Model c s (Either String ())
importEvents = do
  result <- io $ downloadString "https://wiki.haskell.org/index.php?title=Template:Main/Events&printable=yes"
  case result of
    Left e -> return (Left (show e))
    Right str -> do
      now <- io $ getZonedTime
      case runSoup (parseTags str) (extractEventItems now) of
        Left e -> return (Left e)
        Right items -> do mapM_ (addItem Events) items
                          return (Right ())

-- | Skip to each tweet and extract the items.
extractEventItems :: ZonedTime -> Soup [NewItem]
extractEventItems now = do
  skipTagByNameClass "div" "mw-content-ltr"
  go

  where go = do
          skipTagByName "dl"
          skipTagByName "dt"
          tags <- get
          a <- gotoTagByNameClass "a" "external"
          link <- getAttrib "href" a
          uri <- meither "couldn't parse URI" (parseURI link)
          let title = tagsTxt (takeWhile (not . tagCloseLit "dt") tags)
          items <- go <|> return []
          return $ NewItem
            { niTitle = title
            , niPublished = now
            , niDescription = ""
            , niLink = uri
            } : items
