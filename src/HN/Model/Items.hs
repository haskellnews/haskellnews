-- | Get items from the DB.

module HN.Model.Items where

import HN.Types

import Snap.App

-- | Get items filtered by content source.
getItemsBySource :: Source -> Int -> Model c s [DItem]
getItemsBySource source limit = do
  query ["SELECT id,source,title,added,published,description,link"
        ,"FROM item"
        ,"WHERE source = ?"
        ,"ORDER BY published DESC"
        ,"LIMIT ?"]
        (source,limit)
