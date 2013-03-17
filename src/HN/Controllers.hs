{-# LANGUAGE OverloadedStrings #-}

module HN.Controllers where

import HN.Blaze
import HN.View.Home as V

import Snap.App

-- | Home page.
home :: Controller c s ()
home = view $ V.home

-- | Ouput a view.
view :: Html -> Controller c s ()
view = outputText . renderHtml
