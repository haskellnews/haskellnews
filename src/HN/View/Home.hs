{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | The home page.

module HN.View.Home where

import HN.View
import HN.View.Template

import Data.List.Split
import Data.Time.Relative
import qualified Data.Text as T
import Network.URI

grouped now groups = template "grouped" mempty $ do
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
                    Github ->
                      em $ do when (not (T.null (iDescription item))) $ do
                                toHtml $ iDescription item
                                br
                              agoZoned (iPublished item) now
                    _ -> em $ agoZoned (iPublished item) now

mixed now items = template "mixed" mempty $ do
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
            tr $ do
              td !. "icon" $
                img ! src (if iSource item == HaskellCafe
                              then "http://www.haskell.org/favicon.ico"
                              else toValue (show ((iLink item) { uriPath = "/favicon.ico" })))
                    !. "favicon"
                    ! title (toValue (iSource item))
              td $ do
                a ! href (toValue (show (iLink item))) $ toHtml (iTitle item)
                " — "
                case iSource item of
                  Github ->
                    em $ do when (not (T.null (iDescription item))) $ do toHtml $ iDescription item; " — "
                            agoZoned (iPublished item) now
                  _ -> em $ agoZoned (iPublished item) now

agoZoned t1 t2 = span ! title (toValue (show t1)) $
  toHtml (relativeZoned t1 t2 True)

heading = do
  row $ span12 $ do
    h1 "Haskell News"
    p $ em !. "muted" $ "Updated every 10 minutes."
