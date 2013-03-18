-- | General import procedure.

module HN.Model.Import where

import HN.Model.Feeds
import HN.Model.Soup
import HN.Monads
import HN.System

import Snap.App

-- | Import all ALL THE THINGS.
importEverything :: Model c s ()
importEverything = void $ do
  io $ hSetBuffering stdout NoBuffering
  forM_ [importRedditHaskell
        ,importProggit
        ,importVimeo
        ,importTwitter
        ,importHackage
        ,importHaskellWiki
        ,importGithub
        ,importStackOverflow
        ,importPlanetHaskell
        ,importHaskellCafe
        ,importGooglePlus]
        $ \m -> do
          result <- m
          case result of
            Right{} -> io $ putStrLn "OK."
            Left e -> io $ putStrLn e
