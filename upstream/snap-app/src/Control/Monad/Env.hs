{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Abstraction of environment functions (could be state, could be
--   reader, whatever). Intended to ease migration from Reader/State.

module Control.Monad.Env
  (env)
  where

import Control.Monad.Reader

env :: MonadReader env m => (env -> val) -> m val
env = asks
