-- | General import procedure.

module HN.Model.Import where

import HN.Model.Feeds
import HN.Monads

import Snap.App

-- | Import all ALL THE THINGS.
importEverything :: Model c s ()
importEverything = void $ do
  -- TODO: Handle errors and log them.
  importRedditHaskell
  importProggit
