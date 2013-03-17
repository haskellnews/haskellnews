{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | The home page.

module HN.View.Home where

import HN.View
import HN.View.Template

home items = template "home" mempty $ do
  container $ do
    row $ span12 $ h1 "Haskell News"
    row $ span6 $ do
      h2 "Haskell Reddit"
      table !. "table" $
        forM_ items $ \item ->
          tr $ td $ do
            a ! hrefURI (iLink item) $ toHtml (iTitle item)
