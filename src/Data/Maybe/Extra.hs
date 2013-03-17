{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Maybe.Extra where

-- | When a value is Just, do something with it, monadically.
whenJust :: Monad m => Maybe a -> (a -> m c) -> m ()
whenJust (Just a) m = m a >> return ()
whenJust _         _ = return ()
