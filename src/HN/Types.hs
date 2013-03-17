{-# LANGUAGE MultiParamTypeClasses #-}

module HN.Types where

import HN.Monads

import Data.Text (Text)
import Data.Time (ZonedTime)
import Database.PostgreSQL.Simple
import Network.Mail.Mime (Address)
import Network.URI (URI)
import Snap.App.Types

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

-- | A new item.
data NewItem = NewItem
  { niTitle       :: String
  , niPublished   :: ZonedTime
  , niDescription :: String
  , niLink        :: URI
  } deriving Show

data Source = HaskellReddit
  deriving Show
