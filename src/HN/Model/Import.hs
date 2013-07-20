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
  forM_ [(importRedditHaskell,"importRedditHaskell")
        ,(importProggit,"importProggit")
        ,(importVimeo,"importVimeo")
        ,(importTwitter,"importTwitter")
        ,(importHackage,"importHackage")
        ,(importHaskellWiki,"importHaskellWiki")
        ,(importGithub,"importGithub")
        ,(importStackOverflow,"importStackOverflow")
        ,(importPlanetHaskell,"importPlanetHaskell")
        ,(importHaskellCafe,"importHaskellCafe")
        ,(importGooglePlus,"importGooglePlus")
        ,(importIrcQuotes,"importIrcQuotes")
        ,(importPastes,"importPastes")
        ,(importHaskellLive,"importHaskellLive")]
        $ \(m,op) -> do
          result <- m
          case result of
            Right{} -> return ()
            Left e -> io $ print (op,e)
