{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-orphans #-}

module HN.Types where

import HN.Blaze
import HN.Monads

import Control.Applicative
import Control.Arrow
import Data.Text (Text)
import Data.Time (ZonedTime)
import Data.Typeable
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Network.Mail.Mime (Address)
import Network.URI
import Snap.App.Types
import Snap.App.Cache
import Text.Blaze
import Github.Auth (GithubAuth(..))

--------------------------------------------------------------------------------
-- Basic site types

-- | Site-wide configuration.
data Config = Config
  { configPostgres        :: ConnectInfo
  , configDomain          :: String
  , configAdmin           :: Address
  , configSiteAddy        :: Address
  , configCacheDir        :: FilePath
  , configGithubAuth      :: Maybe GithubAuth
  }

instance AppConfig Config where
  getConfigDomain = configDomain

instance CacheDir Config where
  getCacheDir = configCacheDir

instance AppLiftModel Config PState where
  liftModel action = do
    conn <- env controllerStateConn
    anns <- env controllerState
    conf <- env controllerStateConfig
    let st = ModelState conn anns conf
    io $ runReaderT (runModel action) st

-- | App state.
data PState = PState

--------------------------------------------------------------------------------
-- Items

-- | A new item.
data NewItem = NewItem
  { niTitle       :: String
  , niPublished   :: ZonedTime
  , niDescription :: String
  , niLink        :: URI
  } deriving Show

-- | An item saved in the DB.
data DItem = DItem
  { iId          :: Int
  , iSource      :: Source
  , iTitle       :: Text
  , iAdded       :: ZonedTime
  , iPublished   :: ZonedTime
  , iDescription :: Text
  , iLink        :: URI
  } deriving Show

instance FromRow DItem where
  fromRow = DItem <$> field <*> field <*> field <*> field <*> field <*> field <*> field

--------------------------------------------------------------------------------
-- A source of an item

-- | A source of content.
data Source
  = Reddit
  | Vimeo
  | Twitter
  | Hackage
  | HaskellWiki
  | Github
  | StackOverflow
  | Jobs
  | PlanetHaskell
  | HaskellCafe
  | GooglePlus
  | IrcQuotes
  | Pastes
  | HaskellLive
  | Events
  deriving (Typeable,Show,Eq,Enum)

sourceMapping :: [(Source,Int)]
sourceMapping =
  [(Reddit,1)
  ,(Vimeo,2)
  ,(Twitter,3)
  ,(Hackage,4)
  ,(HaskellWiki,5)
  ,(Github,6)
  ,(StackOverflow,7)
  ,(Jobs,8)
  ,(PlanetHaskell,9)
  ,(HaskellCafe,10)
  ,(GooglePlus,11)
  ,(IrcQuotes,12)
  ,(Pastes,13)
  ,(HaskellLive,14)
  ,(Events,15)
  ]

instance ToMarkup Source where
  toMarkup s =
    a ! href (toValue $ sourceToUrl s) ! target "_blank" $ toHtml (sourceToString s)

instance ToValue Source where
  toValue = toValue . sourceToString

sourceToString :: Source -> String
sourceToString i =
    case i of
      Reddit -> "Reddit"
      Vimeo -> "Vimeo"
      Twitter -> "Twitter"
      Hackage -> "Hackage"
      HaskellWiki -> "HaskellWiki"
      Github -> "GitHub"
      StackOverflow -> "Stack Overflow"
      Jobs -> "Jobs"
      PlanetHaskell -> "Planet Haskell"
      HaskellCafe -> "Haskell-Cafe"
      GooglePlus -> "Google+"
      IrcQuotes -> "IRC Quotes"
      Pastes -> "Pastes"
      HaskellLive -> "Haskell Live"
      Events -> "Events"

sourceToUrl :: Source -> String
sourceToUrl i =
    case i of
      Reddit -> "https://pay.reddit.com/r/haskell"
      Vimeo -> "http://vimeo.com/channels/haskell" -- TODO: Also Galois
      Twitter -> "https://twitter.com/search?q=haskell%20-rugby%20-jewelry%20%23haskell&src=typd"
      Hackage -> "https://hackage.haskell.org/packages/recent"
      HaskellWiki -> "https://www.haskell.org/haskellwiki/Special:RecentChanges"
      Github -> "https://github.com/trending?l=haskell" -- TODO: This isn't the same thing
      StackOverflow -> "http://stackoverflow.com/questions/tagged/haskell?sort=newest"
      Jobs -> "http://www.haskellers.com/jobs"
      PlanetHaskell -> "https://planet.haskell.org/"
      HaskellCafe -> "https://groups.google.com/forum/#!forum/haskell-cafe"
      GooglePlus -> "https://plus.google.com/communities/104818126031270146189"
      IrcQuotes -> "http://ircbrowse.net/haskell"
      Pastes -> "http://lpaste.net/browse"
      HaskellLive -> "http://haskelllive.com/"
      Events -> "https://www.haskell.org/haskellwiki/Template:Main/Events"


instance FromField Source where
  fromField f s = do
    i <- fromField f s
    case lookup i (map (snd &&& fst) sourceMapping) of
      Just r -> pure r
      _ -> returnError ConversionFailed f "invalid content source"

instance ToField Source where
  toField s = toField $ case lookup s sourceMapping of
    Nothing -> error "unable to encode field"
    Just i -> toField i

--------------------------------------------------------------------------------
-- Cache key

data CacheKey
  = Mixed Bool
  | Grouped Bool

instance Key CacheKey where
  keyToString (Mixed b) = "mixed-" ++ (if b
                                          then "embeddable"
                                          else "full") ++ ".html"
  keyToString (Grouped b) = "grouped" ++ (if b
                                             then "embeddable"
                                             else "full") ++ ".html"

--------------------------------------------------------------------------------
-- Misc types

instance FromField URI where
  fromField f s = do
    str <- fromField f s
    case parseURI str of
      Nothing  -> returnError ConversionFailed f "invalid URI"
      Just uri -> pure uri

instance ToField URI where
  toField uri = toField $ show uri
