{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Download and import from HTML soup because the services were too
-- stingy to provide an RSS feed.

module HN.Model.Soup where

import HN.Data
import HN.Monads
import HN.Model
import HN.Model.Items
import HN.Types
import HN.Curl

import Control.Applicative
import Control.Monad.Error
import Data.Time
import Network.URI
import Snap.App
import System.Locale
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match

--------------------------------------------------------------------------------
-- Google+

importGooglePlus :: Model c s (Either String ())
importGooglePlus = do
  io $ putStr "Importing Google+ ... "
  result <- io $ downloadString "https://plus.google.com/communities/104818126031270146189"
  case result of
    Left e -> return (Left (show e))
    Right html ->
      case runSoup (parseTags html) getShares of
        Left e -> return (Left e)
        Right items -> do mapM_ (addItem GooglePlus) items
                          return (Right ())

-- | Get Google+ shares.
getShares :: Soup [NewItem]
getShares = do
  skipTagByName "header"
  skipTagByName "a"
  author <- nextText
  a <- gotoTagByName "a"
  link <- fmap ("https://plus.google.com/" ++) (getAttrib "href" a)
  uri <- meither "couldn't parse URI" (parseURI link)
  tstr <- nextText
  time <- parseDate tstr
  skipTagByName "span"
  skipTagByName "div"
  skipTagByName "div"
  skipTagByName "div"
  desc <- get >>= return . tagsText . takeWhile (not . tagOpen (const True) (any (isInfixOf "+1") . map snd))
  items <- getShares <|> return []
  return $ NewItem
    { niTitle = trim author ++ " shares: " ++ trim (unwords (take 10 (words desc)) ++ " ...")
    , niDescription = desc
    , niLink = uri
    , niPublished = time
    } : items

--------------------------------------------------------------------------------
-- Github

-- | Import a list of repos from Github's “latest created Haskell repos” list.
importGithub :: Model c s (Either String ())
importGithub = do
  io $ putStr "Importing Github ... "
  result <- io $ downloadString "https://github.com/languages/Haskell/created"
  case result of
    Left e -> return (Left (show e))
    Right str ->
      case runSoup (parseTags str) extractItems of
        Left e -> return (Left e)
        Right items -> do mapM_ (addItem Github) items
                          return (Right ())

-- | Skip to the repo list and extract the items.
extractItems :: Soup [NewItem]
extractItems = do
  skipTagByNameAttrs "ul" (any (\(key,value) -> key == "class" && isPrefixOf "repolist" value))
  collectItems

-- | Collect items into a loop. This loops.
collectItems :: Soup [NewItem]
collectItems = do
  skipTagByName "li"
  skipTagByName "h3"
  a <- gotoTagByName "a"
  link <- fmap ("http://github.com" ++) (getAttrib "href" a)
  uri <- meither "couldn't parse URI" (parseURI link)
  name <- nextText
  skipTagByNameAttrs "p" (any (\(key,value) -> key == "class" && "description" == value))
  desc <- nextText
  timetag <- gotoTagByName "time"
  time <- getAttrib "datetime" timetag
  t <- parseGithubTime time
  items <- collectItems <|> return []
  return $ NewItem
    { niTitle = trim name
    , niDescription = trim desc
    , niLink = uri
    , niPublished = t
    } : items

meither e Nothing = throwError e
meither _ (Just x) = return x

--------------------------------------------------------------------------------
-- Functions to make tagsoup suck less

newtype Soup a = Soup { unSoup :: StateT [Tag String] (Either String) a }
  deriving (Monad,Functor,MonadError String,MonadState [Tag String],Alternative,Applicative)

runSoup :: [Tag String] -> Soup a -> Either String a
runSoup xs m = evalStateT (unSoup m) xs

getAttrib :: String -> Tag String -> Soup String
getAttrib att (TagOpen _ atts) = return (fromMaybe empty $ lookup att atts)
getAttrib _ x                  = throwError ("(" ++ show x ++ ") is not a TagOpen")

gotoTagByNameAttrs :: String -> ([Attribute String] -> Bool) -> Soup (Tag String)
gotoTagByNameAttrs name attrs = do
  xs <- get
  case dropWhile (not . tagOpen (==name) attrs) xs of
    [] -> throwError $ "Unable to find tag " ++ name ++ "."
    (x:xs) -> do put xs
                 return x

gotoTagByName :: String -> Soup (Tag String)
gotoTagByName name = do
  xs <- get
  case dropWhile (not . tagOpen (==name) (const True)) xs of
    [] -> throwError $ "Unable to find tag " ++ name ++ "."
    (x:xs) -> do put xs
                 return x

skipTagByName :: String -> Soup ()
skipTagByName name = void $ gotoTagByName name

skipTagByNameAttrs :: String -> ([Attribute String] -> Bool) -> Soup ()
skipTagByNameAttrs name attrs = void $ gotoTagByNameAttrs name attrs

nextText :: Soup String
nextText = do
  xs <- get
  case xs of
    (TagText x:xs) -> do put xs
                         return x
    _ -> throwError $ "Expected tag content but got something else: " ++ show (take 1 xs)

parseGithubTime :: (ParseTime a) => String -> Soup a
parseGithubTime str =
  case parseTime defaultTimeLocale "%Y-%m-%dT%T%z" str of
    Nothing -> throwError $ "Unable to parse datetime: " ++ str
    Just t -> return t

parseDate :: (ParseTime a) => String -> Soup a
parseDate str =
  case parseTime defaultTimeLocale "%Y-%m-%d" str of
    Nothing -> throwError $ "Unable to parse date: " ++ str
    Just t -> return t

-- | Extract all text content from tags (similar to Verbatim found in HaXml)
tagsText :: [Tag String] -> String
tagsText = intercalate " " . map trim . mapMaybe maybeTagText
