-- | Main entry point.

module Main where

import HN.Config
import HN.Model.Migrations
import HN.Model.Import
import HN.Server
import HN.Types

import Snap.App
import Snap.App.Migrate
import System.Environment

-- | Main entry point.
main :: IO ()
main = do
  cpath:action <- getArgs
  config <- getConfig cpath
  pool <- newPool (configPostgres config)
  let db = runDB () config pool
  case foldr const "" (map (dropWhile (=='-')) action) of
    "create-version" -> db $ migrate True versions
    "migrate"        -> db $ migrate False versions
    "import"         -> db $ importEverything
    _                -> do db $ migrate False versions
                           runServer config pool
