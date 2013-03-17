{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-name-shadowing #-}

-- | Load the configuration file.

module HN.Config (getConfig) where

import HN.Types

import Data.ConfigFile
import Database.PostgreSQL.Simple (ConnectInfo(..))
import qualified Data.Text as T
import Network.Mail.Mime

getConfig :: FilePath -> IO Config
getConfig conf = do
  contents <- readFile conf
  let config = do
        c <- readstring emptyCP contents
        [pghost,pgport,pguser,pgpass,pgdb]
          <- mapM (get c "POSTGRESQL")
                  ["host","port","user","pass","db"]
        [domain,cache]
          <- mapM (get c "WEB")
                  ["domain","cache"]
        [admin,siteaddy]
          <- mapM (get c "ADDRESSES")
	     	  ["admin","site_addy"]

        return Config {
           configPostgres = ConnectInfo pghost (read pgport) pguser pgpass pgdb
         , configDomain = domain
	 , configAdmin = Address Nothing (T.pack admin)
	 , configSiteAddy = Address Nothing (T.pack siteaddy)
	 , configCacheDir = cache
         }
  case config of
    Left cperr -> error $ show cperr
    Right config -> return config
