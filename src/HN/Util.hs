{-# LANGUAGE OverloadedStrings #-}
module HN.Util where

import Github.Auth as Github (GithubAuth(..))
import qualified Github.Data as Github
import qualified Github.Search as Github
import Data.List (intercalate)
import GHC.Exts (sortWith,groupWith)
import Data.Maybe (fromMaybe,listToMaybe)
import System.Locale
import Data.Time
import qualified Data.Time.Format as DT
import qualified Data.Char as C
import Data.Word (Word8)
import Text.Printf (printf)
import Control.Concurrent (threadDelay)
import qualified Control.Exception as E
import qualified Data.ByteString as BS
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Network.HTTP.Types as W
import Network.HTTP.Conduit (HttpException(..))

data RateLimit = RateLimit { rateLimitLimit :: Int, rateLimitRemaining :: Int, rateLimitReset :: Int } deriving (Show)

allPages :: Show a => (String -> IO (Either a Github.SearchReposResult)) -> String -> IO [[Github.Repo]]
allPages q basequery = go Nothing (1::Int) []
  where
    go needed pageno results =
      if maybe False (<= 0) needed
        then return results
        else do
          reply <- q (basequery ++ "&per_page=100&page=" ++ show pageno)
          case reply of
            Left e -> do
              putStrLn $ "Error for page " ++ show pageno ++ ": " ++ show e
              return results
            Right repos -> do
              -- putStrLn $ "count for page " ++ show pageno ++ ": " ++ show count
              if count >= 100
                then go needed' (pageno+1) (page_repos:results)
                else return (page_repos:results)
              where
                needed' = Just $ maybe total_count (subtract count) needed
                total_count = Github.searchReposTotalCount repos
                page_repos = Github.searchReposRepos repos
                count = length page_repos

baseQuery :: String -> String  -> UTCTime -> String
baseQuery language field utcTime =
  let date = formatUTC utcTime
  in "q=" ++ "language%3A" ++ language ++ " " ++ field ++ "%3A>" ++ date 

doQuery :: Maybe GithubAuth -> String -> IO (Either Github.Error Github.SearchReposResult)
doQuery auth query = handleRateLimit $ Github.searchRepos' auth query

formatRepo :: Github.Repo -> String
formatRepo r =
  let fields = [ ("Name", Github.repoName)
                 ,("URL",  Github.repoHtmlUrl)
                 ,("Description", orEmpty . Github.repoDescription)
                 ,("Created-At", formatMaybeDate . Github.repoCreatedAt)
                 ,("Pushed-At", formatMaybeDate . Github.repoPushedAt)
               ]
  in intercalate "\n" $ map fmt fields
    where fmt (s,f) = fill 12 (s ++ ":") ++ " " ++ f r
          orEmpty = fromMaybe ""
          fill n s = s ++ replicate n' ' '
            where n' = max 0 (n - length s)

formatRepoHtml :: Github.Repo -> [Char]
formatRepoHtml r =
  "<li>" ++ "<a href=\"" ++ url ++ "\"><br/>" ++ name ++ "</a> -- " ++ desc
  where
    url = Github.repoHtmlUrl r
    name = Github.repoName r
    desc = orEmpty $ Github.repoDescription r
    orEmpty = fromMaybe ""

formatMaybeDate :: Maybe Github.GithubDate -> String
formatMaybeDate = maybe "???" formatDate

formatDate :: Github.GithubDate -> String
formatDate = show . Github.fromGithubDate

pushedRepos :: Maybe Github.GithubAuth -> String -> UTCTime -> IO [ Github.Repo ]
pushedRepos auth language t = do
  results <- allPages (doQuery auth) (baseQuery language "pushed" t)
  let repos = sortWith Github.repoPushedAt (uniqueRepos $ concat $ results)
  return repos

parseTime :: String -> IO UTCTime 
parseTime arg@('-':_) = do
  let delta = read arg :: Integer
  now <- getCurrentTime
  let diff = fromInteger $ delta
  let t = addUTCTime diff now
  return t
parseTime arg =
  let maybet = DT.parseTime defaultTimeLocale "%FT%T" arg in
  case maybet of
    Nothing -> error $ "bad time: " ++ arg
    Just t  -> return t

formatUTC :: UTCTime -> String
formatUTC t =
  let local = utcToLocalTime utc t
      day = localDay local
      (y,m,d) = toGregorian day
      localtime = localTimeOfDay local
      hh = todHour localtime
      mm = todMin localtime
      ss = todSec localtime
   in printf "%d-%02d-%02dT%02d:%02d:%02d" y m d hh mm ((floor ss) :: Int)

uniqueRepos :: [ Github.Repo ] -> [ Github.Repo ]
uniqueRepos rs =
  let sorted = sortWith Github.repoHtmlUrl rs
      grouped = groupWith Github.repoHtmlUrl sorted
  in map head grouped

handleRateLimit :: IO (Either Github.Error t) -> IO (Either Github.Error t)
handleRateLimit io = do
  result <- io
  case result of
    Left e  ->
      case rateLimit e of
        Nothing -> return result
        Just rl -> do putStrLn $ "rate limited: " ++ show rl
                      now <- getCurrentTime
                      let diff = rateLimitReset rl - (toSecs now)
                      putStrLn $ "need to sleep for " ++ show diff
                      sleepSeconds diff
                      handleRateLimit io
    _ -> return result

toSecs :: UTCTime -> Int
toSecs = round . utcTimeToPOSIXSeconds

sleepSeconds :: Int -> IO()
sleepSeconds n = threadDelay (n*1000000)

-- | return the rate limit details from an error
rateLimit :: Github.Error -> Maybe RateLimit
rateLimit e =
  case e of
    Github.HTTPConnectionError except ->
      case E.fromException except of
        Just (StatusCodeException (W.Status 403 _) headers _) -> Just $ RateLimit limit remaining reset
          where limit     = findHeader headers "X-RateLimit-Limit"
                remaining = findHeader headers "X-RateLimit-Remaining"
                reset     = findHeader headers "X-RateLimit-Reset"
        _ -> Nothing
    _ -> Nothing

findHeader :: Eq a => [(a, BS.ByteString)] -> a -> Int
findHeader hs key =
  case lookup key hs of
    Nothing -> 0
    Just x  -> fromMaybe 0 (maybeRead (map toChar $ BS.unpack x))

toChar :: Word8 -> Char
toChar = C.chr . fromIntegral

-- | Attempts to parse a value from the front of the string.
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads
