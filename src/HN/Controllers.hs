{-# LANGUAGE OverloadedStrings #-}

module HN.Controllers where

import HN.Blaze
import HN.Monads
import HN.Model.Items
import HN.Types
import HN.Data
import HN.View.Home as V

import Snap.App
import Snap.App.Cache

-- | Grouped display.
grouped :: Controller Config PState ()
grouped = viewCached Grouped $ do
  groups <- forM [Reddit,HaskellCafe,StackOverflow,Github,PlanetHaskell,GooglePlus,Twitter,Hackage,IrcQuotes,Vimeo,HaskellWiki] $ \source -> do
    items <- model $ getItemsBySource source 10
    return (source,items)
  now <- io getZonedTime
  return $ V.grouped now groups

-- | Mixed display.
mixed :: Controller Config PState ()
mixed = viewCached Mixed $ do
  items <- model $ getItems 100
  now <- io getZonedTime
  return $ V.mixed now items
