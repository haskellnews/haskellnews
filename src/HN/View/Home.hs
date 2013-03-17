{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | The home page.

module HN.View.Home where

import HN.View
import HN.View.Template

import Data.List.Split

grouped groups = template "grouped" mempty $ do
  container $ do
    heading
    row $
      span12 $
        ul !. "nav nav-pills" $ do
          li !. "active" $ a "Grouped"
          li $ a ! href "/mixed" $ "Mixed"
    forM_ (chunksOf 2 groups) $ \items ->
      row $
        forM_ items $ \(source,items) ->
          span6 $ do
            h2 $ toHtml source
            table !. "table" $
              forM_ items $ \item ->
                tr $ td $ do
                  a ! href (toValue (show (iLink item))) $ toHtml (iTitle item)
                  " — "
                  case iSource item of
                    Hackage -> do
                      preEscapedText (iDescription item)
                    Github ->
                      em $ do toHtml $ iDescription item
                              br
                              toHtml (show (iPublished item))
                    _ -> em $ toHtml (show (iPublished item))

mixed items = template "mixed" mempty $ do
  container $ do
    heading
    row $
      span12 $
        ul !. "nav nav-pills" $ do
          li $ a ! href "/grouped" $ "Grouped"
          li !. "active" $ a "Mixed"
    row $
      span12 $
        table !. "table" $
          forM_ items $ \item ->
            tr $ td $ do
              a ! href (toValue (show (iLink item))) $ toHtml (iTitle item)
              " — "
              case iSource item of
                Hackage -> do
                  preEscapedText (iDescription item)
                Github ->
                  em $ do toHtml $ iDescription item
                          br
                          toHtml (show (iPublished item))
                _ -> em $ toHtml (show (iPublished item))

heading = do
  row $ span12 $ do
    h1 "Haskell News"
    p $ em !. "muted" $ "Updated every 10 minutes."
