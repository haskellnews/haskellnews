{-# LANGUAGE FlexibleContexts #-}
module Control.Monad.CatchIO.Try
  (
    tryIO
  , eitherIO
{-  
  , MonadCatchIO
  , MonadIO
  , liftIO
  , MonadError
  , Error
  , ErrorType
  , throwError
  , strMsg
  , ErrorT
  , runErrorT-}
  ) where


import           Control.Exception         (IOException)
import           Control.Monad.CatchIO     (MonadCatchIO,tryJust)
import           Control.Monad.Trans.Error (strMsg)
import           Control.Monad.Error       (MonadError,Error,ErrorType,throwError,MonadIO,liftIO)


tryIO :: (Error (ErrorType m),MonadError m,MonadCatchIO m,Functor m) => IO a -> m a
tryIO = (=<<) (either (throwError . strMsg . show) return) . eitherIO . liftIO

eitherIO :: (MonadCatchIO m,Functor m) => m a -> m (Either IOException a)
eitherIO = tryJust (Just :: IOException -> Maybe IOException)
