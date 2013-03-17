{-# LANGUAGE OverloadedStrings #-}

module HN.Controllers where

import HN.Blaze
import HN.Monads
import HN.Model.Items
import HN.Types
import HN.Data
import HN.View.Home as V

import Snap.App

-- | Grouped display.
grouped :: Controller Config PState ()
grouped = do
  groups <- forM [Reddit,HaskellCafe,StackOverflow,Github,PlanetHaskell,Twitter,Hackage,Vimeo,HaskellWiki] $ \source -> do
    items <- model $ getItemsBySource source 10
    return (source,items)
  now <- io getZonedTime
  view $ V.grouped now groups

-- | Mixed display.
mixed :: Controller Config PState ()
mixed = do
  items <- model $ getItems 100
  now <- io getZonedTime
  view $ V.mixed now items

-- | Ouput a view.
view :: Html -> Controller c s ()
view = outputText . renderHtml
