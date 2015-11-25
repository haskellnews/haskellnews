-- | Migration library

module Snap.App.Migrate where

import Snap.App.Types
import Snap.App.Model
import Control.Monad
import Control.Monad.Trans
import GHC.Int

-- | Migrate the DB to the latest version.
migrate :: Bool -> [(Int,Model c s Int64)] -> Model c s ()
migrate create versions = go where
  go = do
    when create $ void $ ensureExists
    rows <- query ["SELECT version FROM version"] ()
    case rows of
      [] -> do echo "No database version, initializing to version 0."
               createVersion
               setVersion 0
      [Only v] -> do
        case lookup (v+1) versions of
          Just doMigrate -> do echo $ "Migrating to version " ++ show (v+1)
                               changes <- doMigrate
                               setVersion (v+1)
                               echo $ "Rows changed: " ++ show changes
                               go
          Nothing -> echo $ "At version " ++ show v ++ "."
      vs -> error $ "There is more than one database version, fix it: " ++
                     show vs

-- | Set the current database version.
setVersion :: Int -> Model c s ()
setVersion v = do
  _ <- exec ["UPDATE version SET version = ?"] (Only v)
  echo $ "Version set to " ++ show v ++ "."
  return ()

-- | Ensure the version table exists.
ensureExists :: Model c s ()
ensureExists = do
  _ <- exec ["CREATE TABLE version (version int not null default 0)"] ()
  return ()

-- | Create the version number.
createVersion :: Model c s ()
createVersion = do
  _ <- exec ["INSERT INTO version (version) VALUES (0)"] ()
  echo $ "Version table created."

-- | Just print to stdout for now.
echo :: String -> Model c s ()
echo = liftIO . putStrLn
