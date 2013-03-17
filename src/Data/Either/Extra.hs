{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Either.Extra where

-- | When a value is Right, do something with it, monadically.
whenRight :: Monad m => Either a b -> (b -> m c) -> m ()
whenRight (Right x) m = m x >> return ()
whenRight _         _ = return ()

-- | When a value is Left, do something with it, monadically.
whenLeft :: Monad m => Either a b -> (a -> m c) -> m ()
whenLeft (Left x) m = m x >> return ()
whenLeft _         _ = return ()
