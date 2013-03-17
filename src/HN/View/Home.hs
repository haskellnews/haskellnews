{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | The home page.

module HN.View.Home where

import HN.View
import HN.View.Template

home = template "home" mempty $ do
  div !. "container" $ do
    h1 "Haskell News"
    p "Hello, World!"
  footer
