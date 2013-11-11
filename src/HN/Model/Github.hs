module HN.Model.Github (
  importGithubPushes
) where

import HN.Monads
import HN.Types
import qualified HN.Util as U
import Github.Util (GithubAuth(..))
import qualified Github.Data as G
import Network.URI (parseURI)
import qualified Data.Time.LocalTime  as LT
import Snap.App
import Data.Maybe (fromMaybe)

importGithubPushes :: Maybe GithubAuth -> String -> Model c s ()
importGithubPushes auth timeSpec = do
  t <- io $ U.parseTime timeSpec
  repos <- io $ U.pushedRepos auth "haskell" t
  forM_ repos (\r -> do
    case repoToItem r of
      Just item -> addItem2 Github item
      Nothing   -> return ()
    )

toZonedTime :: G.GithubDate -> LT.ZonedTime
toZonedTime gd = zt
  where zt = LT.ZonedTime localTime LT.utc
        localTime = LT.utcToLocalTime LT.utc ut
        ut = G.fromGithubDate gd

repoToItem :: G.Repo -> Maybe NewItem
repoToItem repo = do
  uri <- parseURI $ G.repoHtmlUrl repo
  published <- toZonedTime `fmap` G.repoPushedAt repo
  let title = ownerLogin repo ++ "/" ++ G.repoName repo
  let descr = fromMaybe "???" $ G.repoDescription repo
  return $ NewItem title published descr uri

ownerLogin :: G.Repo -> String
ownerLogin = G.githubOwnerLogin . G.repoOwner

addItem2 :: Source -> NewItem -> Model c s ()
addItem2 source item = do
  exists <- itemExists2 source item
  if exists
    then return ()
    else insertItem source item

itemExists2 :: Source -> NewItem -> Model c s (Bool)
itemExists2 source item =  do
  exists <- single ["SELECT true"
                   ,"FROM item"
                   ,"WHERE source = ?"
                   ,"AND   link = ?"
                   ,"AND   published = ?"]
                   (source
                   ,niLink item
                   ,niPublished item)
  case exists :: Maybe Bool of
    Just{} -> return True
    Nothing -> return False

insertItem :: Source -> NewItem -> Model c s ()
insertItem source item = void $
  exec ["INSERT INTO item"
           ,"(source,published,title,description,link)"
           ,"VALUES"
           ,"(?,?,?,?,?)"]
           (source
           ,niPublished item
           ,niTitle item
           ,niDescription item
           ,niLink item)
