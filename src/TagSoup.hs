{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Functions to make tagsoup suck less.

module TagSoup where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.State
import Data.Char
import Data.List
import Data.Maybe
import Data.Time
import Network.URI
import System.Locale
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match

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

parseEpoch :: (ParseTime a) => String -> Soup a
parseEpoch str =
  case parseTime defaultTimeLocale "%s" str of
    Nothing -> throwError $ "Unable to parse epoch seconds to date: " ++ str
    Just t -> return t

-- | Extract all text content from tags (similar to Verbatim found in HaXml)
tagsText :: [Tag String] -> String
tagsText = intercalate " " . map trim' . mapMaybe maybeTagText

-- | Extract all text content from tags.
tagsTxt :: [Tag String] -> String
tagsTxt = concat . mapMaybe maybeTagText

trim' :: String -> String
trim' = dropWhile isSpace . reverse . dropWhile isSpace . reverse

skipTagByNameClass name cls =
  skipTagByNameAttrs name
                     (any (\(key,value) -> key == "class" && any (==cls) (words value)))

gotoTagByNameClass name cls =
  gotoTagByNameAttrs name
                     (any (\(key,value) -> key == "class" && any (==cls) (words value)))

meither e Nothing = throwError e
meither _ (Just x) = return x
