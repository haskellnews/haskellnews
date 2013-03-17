module HN.Model where

import HN.Config
import HN.Types
import HN.Model.Migrations

import Snap.App
import Snap.App.Migrate

-- | Debug function for doing stuff with the database. Aimed at REPL use.
dodb :: Model Config PState () -> IO ()
dodb m = do
  config <- getConfig "../haskellnews.conf"
  pool <- newPool (configPostgres config)
  runDB PState config pool m

-- | Run the migrate function. Also to be used from the REPL.
domigrate :: IO ()
domigrate = dodb $ migrate False versions
