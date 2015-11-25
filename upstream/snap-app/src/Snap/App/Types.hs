{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS -Wall #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Model-view-controller app types.

module Snap.App.Types
       (Controller(..)
       ,Model(..)
       ,ControllerState(..)
       ,ModelState(..)
       ,AppConfig(..)
       ,AppLiftModel(..))
       where

import Control.Applicative        (Applicative,Alternative)
import Control.Monad              (MonadPlus)
import Control.Monad.Catch        (MonadCatchIO)
import Control.Monad.Reader       (ReaderT,MonadReader)
import Control.Monad.Trans        (MonadIO)
import Database.PostgreSQL.Simple (Connection)

import Snap.Core                  (Snap,MonadSnap)

-- | The state accessible to the controller (DB/session stuff).
data ControllerState config state = ControllerState {
    controllerStateConfig :: config
  , controllerStateConn   :: Connection
  , controllerState       :: state
  }

-- | The controller monad.
newtype Controller config state a = Controller {
    runController :: ReaderT (ControllerState config state) Snap a
  } deriving (Monad
             ,Functor
             ,Applicative
             ,Alternative
             ,MonadReader (ControllerState config state)
             ,MonadSnap
             ,MonadIO
             ,MonadPlus
             ,MonadCatchIO)

-- | The state accessible to the model (just DB connection).
data ModelState config state = ModelState {
    modelStateConn   :: Connection
  , modelStateAnns   :: state
  , modelStateConfig :: config
  }

-- | The model monad (limited access to IO, only DB access).
newtype Model config state a = Model {
    runModel :: ReaderT (ModelState config state) IO a
  } deriving (Monad,Functor,Applicative,MonadReader (ModelState config state),MonadIO)

-- -- | Pagination data.
-- data Pagination = Pagination {
--    pnPage :: Integer
--  , pnLimit :: Integer
--  , pnURI :: URI
--  , pnResults :: Integer
--  , pnTotal :: Integer
-- } deriving Show

class AppConfig config where
  getConfigDomain :: config -> String

class AppLiftModel c s where
  liftModel :: Model c s a -> Controller c s a
