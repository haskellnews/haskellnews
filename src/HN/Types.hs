{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS -fno-warn-orphans #-}

module HN.Types where

import HN.Monads

import Control.Applicative
import Data.Typeable
import Data.Text (Text)
import Data.Time (ZonedTime)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromRow
import Network.Mail.Mime (Address)
import Network.URI
import Snap.App.Types

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
  = HaskellReddit
  deriving (Typeable,Show)

instance FromField Source where
  fromField f s = do
    i <- fromField f s
    case i :: Int of
      1 -> pure HaskellReddit
      _ -> returnError ConversionFailed f "invalid content source"

instance ToField Source where
  toField s = toField $ case s of
    HaskellReddit -> toField (1 :: Int)

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
