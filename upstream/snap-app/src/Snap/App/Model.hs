{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Model running.

module Snap.App.Model
  (model
  ,runDB
  ,query
  ,single
  ,singleNoParams
  ,queryNoParams
  ,withPoolConnection
  ,exec
  ,DB.Only(..)
  ,newPool
  ,Pool)
  where

import           Control.Concurrent
import           Control.Monad.CatchIO as E
import           Control.Monad.Env                       (env)
import           Control.Monad.Reader
import           Data.String
import qualified Database.PostgreSQL.Simple as DB
import           Database.PostgreSQL.Simple hiding (query)
import           GHC.Int
import           Snap.App.Types

-- | Run a model action at the top-level.
runDB :: s -> c -> Pool -> Model c s () -> IO ()
runDB st conf pool mdl = do
  withPoolConnection pool $ \conn -> do
      let state = ModelState conn st conf
      -- Default to HTML, can be overridden.
      runReaderT (runModel mdl) state

-- | Run a model action from within a controller.
model :: AppLiftModel c s => Model c s a -> Controller c s a
model = liftModel

-- | Query with some parameters.
query :: (ToRow ps,FromRow r) => [String] -> ps -> Model c s [r]
query q ps = do
  conn <- env modelStateConn
  Model $ ReaderT (\_ -> DB.query conn (fromString (unlines q)) ps)

-- | Query a single field from a single result.
single :: (ToRow ps,FromRow (Only r)) => [String] -> ps -> Model c s (Maybe r)
single q ps = do
  rows <- query q ps
  case rows of
    [(Only r)] -> return (Just r)
    _          -> return Nothing

-- | Query a single field from a single result (no params).
singleNoParams :: (FromRow (Only r)) => [String] -> Model c s (Maybe r)
singleNoParams q = do
  rows <- queryNoParams q
  case rows of
    [(Only r)] -> return (Just r)
    _          -> return Nothing

-- | Query with no parameters.
queryNoParams :: (FromRow r) => [String] -> Model c s [r]
queryNoParams q = do
  conn <- env modelStateConn
  Model $ ReaderT (\_ -> DB.query_ conn (fromString (unlines q)))

-- | Execute some SQL returning the rows affected.
exec :: (ToRow ps) => [String] -> ps -> Model c s Int64
exec q ps = do
  conn <- env modelStateConn
  Model $ ReaderT (\_ -> DB.execute conn (fromString (unlines q)) ps)

-- | Create a new connection pool.
newPool :: MonadIO m
        => ConnectInfo -- ^ Connect info.
        -> m Pool
newPool info = liftIO $ do
  var <- newMVar $ PoolState {
    poolConnections = []
  , poolConnectInfo = info
  }
  return $ Pool var

-- | Connect using the connection pool.
pconnect :: MonadIO m => Pool -> m Connection
pconnect (Pool var) = liftIO $ do
  modifyMVar var $ \state@PoolState{..} -> do
    case poolConnections of
      []           -> do conn <- connect poolConnectInfo
                         return (state,conn)
      (conn:conns) -> return (state { poolConnections = conns },conn)

-- | Restore a connection to the pool.
restore :: MonadIO m => Pool -> Connection -> m ()
restore (Pool var) conn = liftIO $ do
  modifyMVar_ var $ \state -> do
    return state { poolConnections = conn : poolConnections state }

-- | Use the connection pool.
withPoolConnection :: (MonadCatchIO m,MonadIO m) => Pool -> (Connection -> m a) -> m ()
withPoolConnection pool m = do
  _ <- E.bracket (pconnect pool) (restore pool) m
  return ()

-- | A connection pool.
data PoolState = PoolState {
    poolConnections :: [Connection]
  , poolConnectInfo :: ConnectInfo
  }

newtype Pool = Pool (MVar PoolState)
