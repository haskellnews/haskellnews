-- | Get items from the DB.

module HN.Model.Items where

import HN.Types
import HN.Monads

import Snap.App

-- | Get items filtered by content source.
getItemsBySource :: Source -> Int -> Model c s [DItem]
getItemsBySource source limit =
  query ["SELECT id,source,title,added,published,description,link"
        ,"FROM item"
        ,"WHERE source = ?"
        ,"ORDER BY published DESC"
        ,"LIMIT ?"]
        (source,limit)

-- | Get all items.
getItems :: Int -> Model c s [DItem]
getItems limit =
  query ["SELECT id,source,title,added,published,description,link"
        ,"FROM item"
        ,"ORDER BY published DESC"
        ,"LIMIT ?"]
        (Only limit)

-- | Insert an item, if it doesn't already exist.
addItem :: Source -> NewItem -> Model c s ()
addItem source item = do
  exists <- single ["SELECT true"
                   ,"FROM item"
                   ,"WHERE source = ?"
                   ,"AND   title = ?"
                   ,"AND   link = ?"]
                   (source
                   ,niTitle item
                   ,niLink item)
  case exists :: Maybe Bool of
    Just{} -> return ()
    Nothing -> void $
      exec ["INSERT INTO item"
           ,"(source,published,title,description,link)"
           ,"VALUES"
           ,"(?,?,?,?,?)"]
           (source
           ,niPublished item
           ,niTitle item
           ,niDescription item
           ,niLink item)
