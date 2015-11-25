{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}

-- | Simple pagination support for blaze.

module Text.Blaze.Pagination
  (pagination,PN(..))
  where

import           Data.Foldable hiding (foldr)
import           Control.Monad hiding (forM_)
import           Data.Monoid.Operator
import           Data.Pagination
import           Network.URI
import qualified Prelude                     as P
import           Prelude                     hiding ((++),div,span)
import           Text.Blaze.Extra
import           Text.Blaze.Html5            as H hiding (map)

data PN = PN { pnURI :: URI
             , pnPn :: Pagination
             , pnResultsPerPage :: Maybe [Integer]
             }

-- | Render pagination as html.
pagination :: PN -> Html
pagination PN{pnURI=uri, pnPn=pn@Pagination{..}, pnResultsPerPage=perPage} =
  div !. "pagination" $ do
    when pnShowDesc description
    forM_ perPage $ resultsPerPage
    chooser

  where description = do
         p !. "description" $ do
           "Showing "
           toHtml (showCount ((pnCurrentPage-1)*pnPerPage + 1))
           "–"
           toHtml (showCount (min pnTotal (pnCurrentPage * pnPerPage)))
           " of "
           toHtml (showCount (pnTotal))
           " results"

        resultsPerPage perPage = do
          div !. "results-per-page" $ do
            "Page size: "
            forM_ perPage $ \count ->
              span !. "per-page-choice" $ do
                let theclass = if count == pnPerPage then "current" else ""
                if count == pnPerPage
                   then a !. theclass $ toHtml (show count)
                   else a !. theclass ! hrefSet uri (param "per_page") (show count) $
                          toHtml (show count)

        chooser = do
          div !. "pages" $ do
            ul !. "pages-list" $ do
              when (pnCurrentPage > 1) $ do
                li !. "page" $ a ! hrefSet uri paramName "1" $
                  "First"
                li !. "page" $ a ! hrefSet uri paramName (show (pnCurrentPage-1)) $
                  "Previous"
              let w = 10 :: Integer
                  start = max 1 (pnCurrentPage - (w // 2))
                  end = min (pageCount) (start + w)
              forM_ [start..end] $ \i -> do
                let theclass = if i == pnCurrentPage then "active" else ""
                li !. theclass $ do
                  a ! hrefSet uri paramName (show i) !. theclass $
                    toHtml (showCount i)
              when (end < pageCount) $
                li !. "disabled" $ a "…"
              when (pnCurrentPage < pageCount) $ do
                li !. "page" $ a ! hrefSet uri paramName (show (pnCurrentPage+1)) $
                  "Next"
                li !. "page" $ a ! hrefSet uri paramName (show pageCount) $
                  "Last"

        paramName = param "page"
        param p = pnName ++ "_" ++ p

        (//) = P.div
        pageCount = pnPageCount pn

showCount :: (Show n,Integral n) => n -> String
showCount = reverse . foldr merge "" . zip ("000,00,00,00"::String) . reverse . show where
  merge (f,c) rest | f == ',' = "," ++ [c] ++ rest
                   | otherwise = [c] ++ rest
