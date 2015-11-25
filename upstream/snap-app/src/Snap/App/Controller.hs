{-# LANGUAGE BangPatterns #-}
{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Controller routing/handling.

module Snap.App.Controller
  (runHandler
  ,output
  ,outputText
  ,goHome
  ,justOrGoHome
  ,getInteger
  ,getString
  ,getStringMaybe
  ,getPagination
  ,getMyURI)
  where

import Snap.Core
import Snap.App.Model (Pool,withPoolConnection)
import Snap.App.Types

import Control.Applicative
import Control.Monad.Env
import Control.Monad.Reader       (runReaderT)
import Data.ByteString            (ByteString)
import Data.ByteString.UTF8       (toString)
import Data.String
import Data.Pagination
import Network.URI
import Data.Text.Lazy             (Text,toStrict)
import Safe                       (readMay)
import Text.Blaze                 (Markup)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Pagination (PN(..))

-- | Run a controller handler.
runHandler :: s -> c -> Pool -> Controller c s () -> Snap ()
runHandler st conf pool ctrl = do
  withPoolConnection pool $ \conn -> do
      let state = ControllerState conf conn st
      -- Default to HTML, can be overridden.
      modifyResponse $ setContentType "text/html"
      runReaderT (runController ctrl) state

-- | Strictly renders HTML to Text before outputting it via Snap.
--   This ensures that any lazy exceptions are caught by the Snap
--   handler.
output :: Markup -> Controller c s ()
output html = outputText $ renderHtml $ html

-- | Strictly renders text before outputting it via Snap.
--   This ensures that any lazy exceptions are caught by the Snap
--   handler.
outputText :: Text -> Controller c s ()
outputText text = do
  let !x = toStrict $ text
  writeText x

-- | Generic redirect to home page.
goHome :: Controller c s ()
goHome = redirect "/"

-- | Extract a Just value or go home.
justOrGoHome :: Maybe a -> (a -> Controller c s ()) -> Controller c s ()
justOrGoHome x m = maybe goHome m x

-- | Get integer parmater.
getInteger :: ByteString -> Integer -> Controller c s Integer
getInteger name def = do
  pid <- (>>= readMay . toString) <$> getParam name
  maybe (return def) return pid

-- | Get string.
getString :: ByteString -> String -> Controller c s String
getString name def = do
  pid <- (>>= return . toString) <$> getParam name
  maybe (return def) return pid

-- | Get string (maybe).
getStringMaybe :: ByteString -> Controller c s (Maybe String)
getStringMaybe name = do
  pid <- (>>= return . toString) <$> getParam name
  return pid

-- | Get pagination data.
getPagination :: AppConfig c => String -> Controller c s PN
getPagination name = do
  p <- getInteger (fromString (name ++ "_page")) 1
  limit <- getInteger (fromString (name ++ "_per_page")) 35
  uri <- getMyURI
  let pag = Pagination { pnCurrentPage = max 1 p
                       , pnPerPage = max 1 (min 100 limit)
                       , pnTotal = 0
                       , pnName = name
                       , pnShowDesc = True
                       }
  return (PN uri pag Nothing)

getMyURI :: AppConfig c => Controller c s URI
getMyURI = do
  domain <- env (getConfigDomain . controllerStateConfig)
  result <- fmap (parseURI . (("http://" ++ domain) ++) . toString . rqURI)
                 getRequest
  case result of
    Nothing -> case parseURI ("http://" ++ domain) of
      Nothing -> error $ "Unable to parse my own domain! It's this: " ++ domain
      Just d -> return d
    Just d -> return d
