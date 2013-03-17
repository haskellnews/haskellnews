{-# LANGUAGE OverloadedStrings #-}

-- | The web service.

module HN.Server where

import qualified HN.Controllers as C
import           HN.Types

import           Snap.App
import           Snap.Http.Server           hiding (Config)
import           Snap.Util.FileServe

-- | Run the server.
runServer :: Config -> Pool -> IO ()
runServer config pool = do
  setUnicodeLocale "en_US"
  httpServe server (serve config pool)

  where server = setPort 10010 defaultConfig

-- | Serve the controllers.
serve :: Config -> Pool -> Snap ()
serve config pool = route routes where
  routes = [("/js/",serveDirectory "static/js")
           ,("/css/",serveDirectory "static/css")
           ,("/js/",serveDirectory "static/js")
           ,("/mixed",run C.mixed)
           ,("/grouped",run C.grouped)
           ,("/",run C.mixed)
           ]
  run = runHandler PState config pool
