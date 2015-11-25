{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables, MagicHash #-}
-- | Warning: this module is /deprecated/.
-- 
-- Please consider using the package
-- <http://hackage.haskell.org/package/exceptions exceptions>
-- instead, if possible.
-- 
-- The functions @block@ and @unblock@, which are part of the @MonadCatchIO@
-- class, have known problems. The IO instances of these functions, which are
-- provided by the base library, have been deprecated for some time, and have
-- been removed in base-4.7.
module Control.Monad.CatchIO
  (
    MonadCatchIO(..)
  , E.Exception(..)
  , throw
  , try, tryJust
  , Handler(..), catches
  -- * Utilities
  , bracket
  , bracket_
  , bracketOnError
  , finally
  , onException
  )

where

import           Prelude hiding ( catch )
import           Control.Applicative                          ((<$>))
import qualified Control.Exception.Extensible      as E

import           Control.Monad.IO.Class                       (MonadIO,liftIO)

import           Control.Monad.Trans.Cont                     (ContT(ContT)    ,runContT    ,mapContT    )
import           Control.Monad.Trans.Error                    (ErrorT          ,runErrorT   ,mapErrorT   ,Error)
import           Control.Monad.Trans.Identity                 (IdentityT       ,runIdentityT,mapIdentityT)
import           Control.Monad.Trans.List                     (ListT(ListT)    ,runListT    ,mapListT    )
import           Control.Monad.Trans.Maybe                    (MaybeT          ,runMaybeT   ,mapMaybeT   )
import           Control.Monad.Trans.RWS                      (RWST(RWST)      ,runRWST     ,mapRWST     )
import qualified Control.Monad.Trans.RWS.Strict    as Strict  (RWST(RWST)      ,runRWST     ,mapRWST     )
import           Control.Monad.Trans.Reader                   (ReaderT(ReaderT),runReaderT  ,mapReaderT  )
import           Control.Monad.Trans.State                    (StateT(StateT)  ,runStateT   ,mapStateT   )
import qualified Control.Monad.Trans.State.Strict  as Strict  (StateT(StateT)  ,runStateT   ,mapStateT   )
import           Control.Monad.Trans.Writer                   (WriterT         ,runWriterT  ,mapWriterT  )
import qualified Control.Monad.Trans.Writer.Strict as Strict  (WriterT         ,runWriterT  ,mapWriterT  )

import           Data.Monoid                                  (Monoid)

import           GHC.Base                                     (maskAsyncExceptions#)
import           GHC.IO                                       (unsafeUnmask,IO(IO))


class MonadIO m => MonadCatchIO m where
  -- | Generalized version of 'E.catch'
  catch   :: E.Exception e => m a -> (e -> m a) -> m a
  block   :: m a -> m a
  unblock :: m a -> m a


instance MonadCatchIO IO where
  catch   = E.catch
  block   = \ (IO io) -> IO $ maskAsyncExceptions# io
  unblock = unsafeUnmask

-- | Warning: this instance is somewhat contentious.
-- 
-- In the same way that the @ErrorT e@ instance may fail to perform the final
-- action, due to the \"early exit\" behaviour of the monad, this instance
-- may perform the final action any number of times, due to the nonlinear
-- nature of the continuation monad.
-- 
-- See the mailing list message
-- <http://web.archiveorange.com/archive/v/nDNOvaYx1poDHZNlmlgh>
-- for an example of what can go wrong (freeing memory twice).
instance MonadCatchIO m => MonadCatchIO (ContT r m) where
  m `catch` f = ContT $ \c -> runContT m c `catch` \e -> runContT (f e) c
  block       = mapContT block
  unblock     = mapContT unblock

-- | Warning: this instance is somewhat contentious.
-- 
-- Note that in monads that fall under this instance (the most basic example
-- is @ErrorT e IO@), there are errors of two sorts:
-- 
-- 1. exceptions, (i.e., exceptional values in the underlying @IO@ monad);
-- 
-- 2. error values of type @e@, introduced by the @ErrorT e@ part of the monad.
-- 
-- The instance takes no special action to deal with errors of type 2.
-- In particular, 'bracket' will not perform its second argument, if
-- its third argument decides to \"exit early\" by throwing an error of type 2.
-- 
-- This may or may not be what you want.
-- 
-- See the mailing list thread starting with
-- <http://www.mail-archive.com/haskell-cafe@haskell.org/msg82859.html>
-- for some details.
instance (MonadCatchIO m, Error e) => MonadCatchIO (ErrorT e m) where
  m `catch` f = mapErrorT (\m' -> m' `catch` \e -> runErrorT $ f e) m
  block       = mapErrorT block
  unblock     = mapErrorT unblock

instance (MonadCatchIO m) => MonadCatchIO (IdentityT m) where
  m `catch` f = mapIdentityT (\m' -> m' `catch` \e -> runIdentityT $ f e) m
  block       = mapIdentityT block
  unblock     = mapIdentityT unblock

instance MonadCatchIO m => MonadCatchIO (ListT m) where
  m `catch` f = ListT $ runListT m `catch` \e -> runListT (f e)
  block       = mapListT block
  unblock     = mapListT unblock

instance (MonadCatchIO m) => MonadCatchIO (MaybeT m) where
  m `catch` f = mapMaybeT (\m' -> m' `catch` \e -> runMaybeT $ f e) m
  block       = mapMaybeT block
  unblock     = mapMaybeT unblock

instance (Monoid w, MonadCatchIO m) => MonadCatchIO (RWST r w s m) where
  m `catch` f = RWST $ \r s -> runRWST m r s `catch` \e -> runRWST (f e) r s
  block       = mapRWST block
  unblock     = mapRWST unblock

instance (Monoid w, MonadCatchIO m) => MonadCatchIO (Strict.RWST r w s m) where
  m `catch` f = Strict.RWST $ \r s -> Strict.runRWST m r s `catch` \e -> Strict.runRWST (f e) r s
  block       = Strict.mapRWST block
  unblock     = Strict.mapRWST unblock

instance MonadCatchIO m => MonadCatchIO (ReaderT r m) where
  m `catch` f = ReaderT $ \r -> runReaderT m r `catch` \e -> runReaderT (f e) r
  block       = mapReaderT block
  unblock     = mapReaderT unblock

instance MonadCatchIO m => MonadCatchIO (StateT s m) where
  m `catch` f = StateT $ \s -> runStateT m s `catch` \e -> runStateT (f e) s
  block       = mapStateT block
  unblock     = mapStateT unblock

instance MonadCatchIO m => MonadCatchIO (Strict.StateT s m) where
  m `catch` f = Strict.StateT $ \s -> Strict.runStateT m s `catch` \e -> Strict.runStateT (f e) s
  block       = Strict.mapStateT block
  unblock     = Strict.mapStateT unblock

instance (Monoid w, MonadCatchIO m) => MonadCatchIO (WriterT w m) where
  m `catch` f = mapWriterT (\m' -> m' `catch` \e -> runWriterT $ f e) m
  block       = mapWriterT block
  unblock     = mapWriterT unblock

instance (Monoid w, MonadCatchIO m) => MonadCatchIO (Strict.WriterT w m) where
  m `catch` f = Strict.mapWriterT (\m' -> m' `catch` \e -> Strict.runWriterT $ f e) m
  block       = Strict.mapWriterT block
  unblock     = Strict.mapWriterT unblock

-- | Generalized version of 'E.throwIO'
throw :: (MonadIO m, E.Exception e) => e -> m a
throw = liftIO . E.throwIO

-- | Generalized version of 'E.try'
try :: (MonadCatchIO m, Functor m, E.Exception e) => m a -> m (Either e a)
try a = catch (Right <$> a) (return . Left)

-- | Generalized version of 'E.tryJust'
tryJust :: (MonadCatchIO m, Functor m, E.Exception e)
        => (e -> Maybe b) -> m a -> m (Either b a)
tryJust p a = do
  r <- try a
  case r of
    Right v -> return (Right v)
    Left  e -> case p e of
      Nothing -> throw e `asTypeOf` return (Left undefined)
      Just b  -> return (Left b)

-- | Generalized version of 'E.Handler'
data Handler m a = forall e . E.Exception e => Handler (e -> m a)

-- | Generalized version of 'E.catches'
catches :: MonadCatchIO m => m a -> [Handler m a] -> m a
catches a handlers = a `catch` handler where
  handler e = foldr tryH (throw e) handlers where
    tryH (Handler h) res = maybe res h $ E.fromException e

-- | Generalized version of 'E.bracket'
bracket :: MonadCatchIO m => m a -> (a -> m b) -> (a -> m c) -> m c
bracket before after thing = block $ do
  a <- before
  r <- unblock (thing a) `onException` after a
  _ <- after a
  return r

-- | Generalized version of 'E.onException'
onException :: MonadCatchIO m => m a -> m b -> m a
onException a onEx = a `catch` (\ (e :: E.SomeException) -> onEx >> throw e)

-- | A variant of 'bracket' where the return value from the first computation
-- is not required.
bracket_ :: MonadCatchIO m
         => m a  -- ^ computation to run first (\"acquire resource\")
         -> m b  -- ^ computation to run last (\"release resource\")
         -> m c  -- ^ computation to run in-between
         -> m c  -- returns the value from the in-between computation
bracket_ before after thing = block $ do
  _ <- before
  r <- unblock thing `onException` after
  _ <- after
  return r

-- | A specialised variant of 'bracket' with just a computation to run
-- afterward.
finally :: MonadCatchIO m
        => m a -- ^ computation to run first
        -> m b -- ^ computation to run afterward (even if an exception was
               -- raised)
        -> m a -- returns the value from the first computation
thing `finally` after = block $ do
  r <- unblock thing `onException` after
  _ <- after
  return r

-- | Like 'bracket', but only performs the final action if there was an
-- exception raised by the in-between computation.
bracketOnError :: MonadCatchIO m
               => m a        -- ^ computation to run first (\"acquire resource\")
               -> (a -> m b) -- ^ computation to run last (\"release resource\")
               -> (a -> m c) -- ^ computation to run in-between
               -> m c        -- returns the value from the in-between computation
bracketOnError before after thing = block $ do
  a <- before
  unblock (thing a) `onException` after a
