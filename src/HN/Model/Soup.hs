-- | Download and import from HTML soup because the services were too
-- stingy to provide an RSS feed.

module HN.Model.Soup where

import HN.Data
import HN.Monads
import HN.Model.Items
import HN.Types
import HN.Curl

import Control.Applicative
import Data.Time
import Network.URI
import System.Locale
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Snap.App

-- | Import a list of repos from Github's “latest created Haskell repos” list.
importGithub :: Model c s (Either String ())
importGithub = do
  result <- io $ downloadString "https://github.com/languages/Haskell/created"
  case result of
    Left e -> return (Left (show e))
    Right str ->
      case extractItems (parseTags str) of
        Left e -> return (Left e)
        Right items -> do mapM_ (addItem Github) items
                          return (Right ())

extractItems xs = do
  ul <- skipTagByNameAttrs "ul" (any (\(key,value) -> key == "class" && isPrefixOf "repolist" value)) xs
  collectItems ul

collectItems ul = do
  li <- skipTagByName "li" ul
  h3 <- skipTagByName "h3" li
  (a,rest) <- gotoTagByName "a" h3
  link <- fmap ("http://github.com" ++) (getAttrib "href" a)
  uri <- meither "couldn't parse URI" (parseURI link)
  (name,rest) <- nextText rest
  rest <- skipTagByNameAttrs "p" (any (\(key,value) -> key == "class" && "description" == value)) rest
  (desc,rest) <- nextText rest
  (timetag,rest) <- gotoTagByName "time" rest
  time <- getAttrib "datetime" timetag
  t <- parseGithubTime time
  items <- collectItems rest <|> return []
  return $ NewItem
    { niTitle = trim name
    , niDescription = trim desc
    , niLink = uri
    , niPublished = t
    } : items

  where trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse
        meither e Nothing = Left e
        meither _ (Just x) = Right x

--------------------------------------------------------------------------------
-- Functions to make tagsoup suck less

getAttrib :: String -> Tag String -> Either String String
getAttrib att (TagOpen _ atts) = return (fromMaybe empty $ lookup att atts)
getAttrib _ x = Left ("(" ++ show x ++ ") is not a TagOpen")

gotoTagByNameAttrs name attrs xs =
  case dropWhile (not . tagOpen (==name) attrs) xs of
    [] -> Left $ "Unable to find tag " ++ name ++ "."
    (x:xs) -> return (x,xs)

gotoTagByName name xs =
  case dropWhile (not . tagOpen (==name) (const True)) xs of
    [] -> Left $ "Unable to find tag " ++ name ++ "."
    (x:xs) -> return (x,xs)

skipTagByName name xs = fmap snd (gotoTagByName name xs)
skipTagByNameAttrs name attrs xs = fmap snd (gotoTagByNameAttrs name attrs xs)

nextText xs =
  case xs of
    (TagText x:xs) -> return (x,xs)
    _ -> Left $ "Expected tag content but got something else: " ++ show (take 1 xs)

parseGithubTime str =
  case parseTime defaultTimeLocale "%Y-%m-%dT%T%z" str of
    Nothing -> Left $ "Unable to parse datetime: " ++ str
    Just t -> return t
