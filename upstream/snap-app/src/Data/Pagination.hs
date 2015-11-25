{-# LANGUAGE RecordWildCards #-}
-- | Data pagination.

module Data.Pagination where

import Data.Default
import Data.Maybe
import Safe
import Network.URI
import Network.URI.Params

-- | A pagination object, holds information about the name, total, per
--   page, current page, etc.
data Pagination = Pagination
  { pnTotal       :: Integer
  , pnPerPage     :: Integer
  , pnName        :: String
  , pnCurrentPage :: Integer
  , pnShowDesc    :: Bool
  } deriving (Show)

instance Default Pagination where
  def = Pagination
        { pnTotal       = 0
        , pnPerPage     = 5
        , pnName        = ""
        , pnCurrentPage = 1
        , pnShowDesc    = True
        }

-- | Get the page count of the pagination results.
pnPageCount :: Pagination -> Integer
pnPageCount Pagination{..} = max 1 $
  if total/perpage > fromIntegral (round (total/perpage))
     then round (total/perpage) + 1
     else round (total/perpage)
  where total = fromIntegral pnTotal
        perpage = fromIntegral pnPerPage

-- | Add the current page of the pagination from the current URI.
addCurrentPNData :: URI -> Pagination -> Pagination
addCurrentPNData uri pagination =
  pagination { pnCurrentPage = currentPage
             , pnPerPage = perPage
             }

    where currentPage = fromMaybe 1 $ do
            p <- lookup (param "page") $ uriParams uri
            readMay p
          perPage = fromMaybe (pnPerPage pagination) $ do
            p <- lookup (param "per_page") $ uriParams uri
            readMay p
          param n = pnName pagination ++ "_" ++ n
