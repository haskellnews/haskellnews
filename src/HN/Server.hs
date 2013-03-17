{-# LANGUAGE OverloadedStrings #-}

-- | The web service.

module HN.Server where

import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe

-- | Run the web server.
runServer :: IO ()
runServer = do
  httpServe (setPort 10010 defaultConfig)
            (route [("/js/",serveDirectory "static/js")
                   ,("/css/",serveDirectory "static/css")
                   ,("/",home)])

-- | Home page.
home :: Snap ()
home = writeText "Hai"
