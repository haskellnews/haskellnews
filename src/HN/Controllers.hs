{-# LANGUAGE OverloadedStrings #-}

module HN.Controllers where

import HN.Blaze
import HN.Model.Items
import HN.Types
import HN.View.Home as V

import Snap.App

-- | Home page.
home :: Controller Config PState ()
home = do
  items <- model $ getItemsBySource HaskellReddit 5
  view $ V.home items

-- | Ouput a view.
view :: Html -> Controller c s ()
view = outputText . renderHtml
