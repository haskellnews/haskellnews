{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | The home page.

module HN.View.Home where

import           HN.View hiding (source, item)
import           HN.View.Template

import           Data.List.Split
import qualified Data.Text as T
import           Data.Time.Relative
import           Network.URI
import           Text.Blaze.Internal (MarkupM)

grouped :: (Foldable t, ToMarkup m) => ZonedTime -> [(m, t DItem)] -> MarkupM ()
grouped now groups = template "grouped" (return ()) $ do
  container $ do
    heading
    row $
      span12 $
        ul !. "nav nav-pills" $ do
          li !. "active" $ a "Grouped"
          li $ a ! href "/mixed" $ "Mixed"
    groupedContent now groups

groupedContent :: (Foldable t, ToMarkup m) => ZonedTime -> [(m, t DItem)] -> MarkupM ()
groupedContent now groups =
  forM_ (chunksOf 2 groups) $ \items ->
    row $
      forM_ items $ \(source,items') ->
        span6 $ do
          h2 $ toHtml source
          table !. "table" $
            forM_ items' $ \item ->
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

mixed :: (Foldable t) => ZonedTime -> t DItem -> MarkupM ()
mixed now items = template "mixed" (return ()) $ do
  container $ do
    heading
    row $
      span12 $
        ul !. "nav nav-pills" $ do
          li $ a ! href "/grouped" $ "Grouped"
          li !. "active" $ a "Mixed"
    mixedContent now items

mixedContent :: Foldable t => ZonedTime -> t DItem -> Html
mixedContent now items =
  do row $
          span12 $ do
            p $ em !. "muted" $ a ! href "/feed" ! target "_blank" $ "Subscribe via RSS"
            table !. "table" $
              mixedRows now items

mixedRows :: Foldable t => ZonedTime -> t DItem -> MarkupM ()
mixedRows now items =
  forM_ items $ \item ->
    tr ! id (toValue ("item-" ++ epoch (iPublished item))) $ do
      td !. "icon" $
        img ! src (toValue (show ((iLink item) { uriPath = "/favicon.ico" })))
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

epoch :: ZonedTime -> String
epoch = formatTime defaultTimeLocale "%s"

agoZoned :: ZonedTime -> ZonedTime -> Html
agoZoned t1 t2 = span !. "relative-time" ! dataAttribute "epoch" (toValue (epoch t1)) ! title (toValue (show t1)) $
  toHtml (relativeZoned t1 t2 True)

heading :: Html
heading = do
  row $ span12 $ do
    h1 "Haskell News"
    p $ em !. "muted" $ "Updated automatically every 10 minutes."
