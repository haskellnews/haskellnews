{-# LANGUAGE OverloadedStrings #-}

module HN.Controllers where

import HN.Blaze
import HN.Monads
import HN.Model.Items
import HN.Types
import HN.View.Home as V

import Snap.App

-- | Home page.
home :: Controller Config PState ()
home = do
  groups <- forM [toEnum 0 ..] $ \source -> do
    items <- model $ getItemsBySource source 5
    return (source,items)
  view $ V.home groups

-- | Ouput a view.
view :: Html -> Controller c s ()
view = outputText . renderHtml
