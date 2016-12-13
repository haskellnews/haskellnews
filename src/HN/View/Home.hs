{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | The home page.

module HN.View.Home where

import           HN.View
import           HN.View.Template

import           Data.List.Split
import qualified Data.Text as T
import           Data.Time.Relative
import           Network.URI

grouped now groups = template "grouped" (return ()) $ do
  container $ do
    heading
    row $
      span12 $
        ul !. "nav nav-pills" $ do
          li !. "active" $ a "Grouped"
          li $ a ! href "/mixed" $ "Mixed"
    groupedContent now groups

groupedContent now groups =
  forM_ (chunksOf 2 groups) $ \items ->
    row $
      forM_ items $ \(source,items) ->
        span6 $ do
          h2 $ toHtml source
          table !. "table" $
            forM_ items $ \item ->
              tr $ td $ do
                a ! href (toValue (show (iLink item))) ! target "_blank" $ toHtml (iTitle item)
                " — "
                case iSource item of
                  Github ->
                    em $ do when (not (T.null (iDescription item))) $ do
                              toHtml $ iDescription item
                              br
                            agoZoned (iPublished item) now
                  _ -> em $ agoZoned (iPublished item) now

mixed now items = template "mixed" (return ()) $ do
  container $ do
    heading
    row $
      span12 $
        ul !. "nav nav-pills" $ do
          li $ a ! href "/grouped" $ "Grouped"
          li !. "active" $ a "Mixed"
    mixedContent now items

mixedContent now items =
  do row $
          span12 $ do
            p $ em !. "muted" $ a ! href "/feed" ! target "_blank" $ "Subscribe via RSS"
            table !. "table" $
              mixedRows now items

mixedRows now items =
  forM_ items $ \item ->
    tr ! id (toValue ("item-" ++ epoch (iPublished item))) $ do
      td !. "icon" $
        img ! src (if iSource item == HaskellCafe
                      then "http://www.haskell.org/favicon.ico"
                      else toValue (show ((iLink item) { uriPath = "/favicon.ico" })))
            !. "favicon"
            ! title (toValue (iSource item))
      td $ do
        a ! href (toValue (show (iLink item))) ! target "_blank" $ toHtml (iTitle item)
        " — "
        case iSource item of
          Github ->
            em $ do when (not (T.null (iDescription item))) $ do toHtml $ iDescription item; " — "
                    agoZoned (iPublished item) now
          _ -> em $ agoZoned (iPublished item) now

epoch = formatTime defaultTimeLocale "%s"

agoZoned t1 t2 = span !. "relative-time" ! dataAttribute "epoch" (toValue (epoch t1)) ! title (toValue (show t1)) $
  toHtml (relativeZoned t1 t2 True)

heading = do
  row $ span12 $ do
    h1 "Haskell News"
    p $ em !. "muted" $ "Updated automatically every 10 minutes."
