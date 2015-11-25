{-# OPTIONS -Wall -fno-warn-name-shadowing #-}

-- | Caching of Blaze HTML pages caching.

module Snap.App.Cache
  (cache
  ,cacheIf
  ,resetCache
  ,clearCache
  ,resetCacheModel
  ,viewCached
  ,Key(..)
  ,CacheDir(..))
  where

import           Control.Monad.Reader
import           Data.Text.Lazy           (Text)
import qualified Data.Text.Lazy.IO as T
import           Snap.App
import           System.Directory
import           System.FilePath
import           Text.Blaze
import           Text.Blaze.Html.Renderer.Text

-- | A key for the cache.
class Key key where
  keyToString :: key -> FilePath

-- | A config that can return a cache directory.
class CacheDir config where
  getCacheDir :: config -> FilePath

-- | Cache conditionally.
cacheIf :: (CacheDir c,Key key) => Bool -> key -> Controller c s (Maybe Markup) -> Controller c s (Maybe Text)
cacheIf pred key generate =
  if pred
     then cache key generate
     else fmap (fmap renderHtml) generate

-- | Generate and save into the cache, or retrieve existing from the
-- | cache.
cache :: (CacheDir c,Key key) => key -> Controller c s (Maybe Markup) -> Controller c s (Maybe Text)
cache key generate = do
  tmpdir <- asks (getCacheDir . controllerStateConfig)
  let cachePath = tmpdir ++ "/" ++ keyToString key
  exists <- io $ doesFileExist cachePath
  if exists
     then do text <- io $ T.readFile cachePath
     	     return (Just text)
     else do text <- fmap (fmap renderHtml) generate
     	     case text of
	       Just text' -> do io $ createDirectoryIfMissing True tmpdir
                                io $ T.writeFile cachePath text'
	       	    	        return text
               Nothing -> return text

-- | Clear the whole cache.
clearCache :: CacheDir c => c -> IO ()
clearCache config = do
  files <- getDirectoryContents dir
  forM_ (filter (not . all (=='.')) files) $ removeFile . (dir </>)

  where dir = getCacheDir config

-- | Reset an item in the cache.
resetCache :: (CacheDir c,Key key) => key -> Controller c s ()
resetCache key = do
  tmpdir <- asks (getCacheDir . controllerStateConfig)
  io $ do
   let cachePath = tmpdir ++ "/" ++ keyToString key
   exists <- io $ doesFileExist cachePath
   when exists $ removeFile cachePath

-- | Reset an item in the cache.
resetCacheModel :: (CacheDir c,Key key) => key -> Model c s ()
resetCacheModel key = do
  tmpdir <- asks (getCacheDir . modelStateConfig)
  io $ do
   let cachePath = tmpdir ++ "/" ++ keyToString key
   exists <- io $ doesFileExist cachePath
   when exists $ removeFile cachePath

-- | Because.
io :: MonadIO m => IO a -> m a
io = liftIO

-- | View some HTML generator cached.
viewCached :: (CacheDir c,Key key) => key -> Controller c s Markup -> Controller c s ()
viewCached key generate = do
  text <- cache key (fmap Just generate)
  maybe (return ()) outputText text
