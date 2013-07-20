{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module HN.Controllers where

import           HN.Monads
import           HN.Model.Items
import           HN.Types
import           HN.Data
import           HN.View.Home as V

import qualified Data.Text as T
import           Snap.App
import           Snap.App.Cache
import           Snap.App.RSS

-- | Grouped display.
grouped :: Controller Config PState ()
grouped = viewCached Grouped $ do
  groups <- forM [Reddit,HaskellCafe,StackOverflow,Pastes,Github,PlanetHaskell,GooglePlus,Twitter,Hackage,IrcQuotes,Vimeo,HaskellWiki] $ \source -> do
    items <- model $ getItemsBySource source 10
    return (source,items)
  now <- io getZonedTime
  return $ V.grouped now groups

-- | Mixed display.
mixed :: Controller Config PState ()
mixed = viewCached Mixed $ do
  items <- model $ getItems 100
  now <- io getZonedTime
  return $ V.mixed now items

-- | Output an RSS feed.
feed :: Controller Config PState ()
feed = do
  items <- model $ getItems 30
  outputRSS "Haskell News"
            "http://haskellnews.org/"
            (map (\DItem{..} -> (zonedTimeToUTC iAdded
                                ,"[" <> T.pack (show iSource) <> "] " <> iTitle
                                ,""
                                ,T.pack $ show iLink))
                 items)

-- | Output items after some number in a page fragment.
after :: Controller Config PState ()
after = do
  n <- getInteger "epoch" 0
  when (n /= 0) $ do
    items <- model $ getItemsAfter (fromIntegral n) 100
    unless (null items) $ do
      now <- io getZonedTime
      output $ V.mixedRows now items
