-- | General import procedure.

module HN.Model.Import where

import HN.Model.Feeds
import HN.Model.Soup
import HN.Monads
import HN.System

import Snap.App

-- | Import ALL THE THINGS.
importEverything :: Model c s ()
importEverything = void $ do
  io $ hSetBuffering stdout NoBuffering
  forM_ [(importRedditHaskell,"importRedditHaskell")
        ,(importRedditHaskellQuestions,"importRedditHaskellQuestions")
        ,(importProggit,"importProggit")
        ,(importVimeo,"importVimeo")
        ,(importHaskellTwitter,"importHaskellTwitter")
        ,(importHaskellTips,"importTips")
        ,(importHackage,"importHackage")
        ,(importHaskellWiki,"importHaskellWiki")
        ,(importStackOverflow,"importStackOverflow")
        ,(importPlanetHaskell,"importPlanetHaskell")
        ,(importHaskellCafeNative,"importHaskellCafeNative")
        ,(importGooglePlus,"importGooglePlus")
        ,(importIrcQuotes,"importIrcQuotes")
        ,(importPastes,"importPastes")
        ,(importEvents,"importEvents")]
        $ \(m,op) -> do
          result <- m
          case result of
            Right{} -> return ()
            Left e -> io $ print (op,e)
