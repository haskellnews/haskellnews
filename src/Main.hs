-- | Main entry point.

module Main where

import HN.Config
import HN.Model.Migrations
import HN.Server
import HN.Types

import Data.Maybe
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

  case foldr const "" action of
    "--create-version" -> db $ migrate True versions
    _ -> do
      db $ migrate False versions
      runServer config pool
