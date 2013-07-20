{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS -fno-warn-orphans #-}

module HN.Types where

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

--------------------------------------------------------------------------------
-- Basic site types

-- | Site-wide configuration.
data Config = Config
  { configPostgres        :: ConnectInfo
  , configDomain          :: String
  , configAdmin           :: Address
  , configSiteAddy        :: Address
  , configCacheDir        :: FilePath
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
  ]

instance ToMarkup Source where
  toMarkup = toMarkup . sourceToString
instance ToValue Source where
  toValue = toValue . sourceToString

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
  = Mixed
  | Grouped

instance Key CacheKey where
  keyToString Mixed = "mixed.html"
  keyToString Grouped = "grouped.html"

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
