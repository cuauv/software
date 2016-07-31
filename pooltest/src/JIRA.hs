module JIRA where

import           Control.Lens
import qualified Data.Aeson            as A
import           Data.Aeson.Lens
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict   as M
import qualified Data.Text             as T
import qualified Network.Wreq          as W

import           Utility

host ∷ String
host = "https://jira.cuauv.org"

urlify ∷ String → String
urlify = (++) (host ++ "/rest/api/2/")

user ∷ B.ByteString
user = "software"

pass ∷ B.ByteString
pass = "Wutz4Dinner?!"

opts ∷ W.Options
opts = W.defaults & W.auth ?~ W.basicAuth user pass

post ∷ String → A.Value → IO A.Value
post resource req = do
  res <- W.asValue =<< W.postWith opts (urlify resource) req
  return $ res ^. W.responseBody

get ∷ String → IO A.Value
get resource = do
  res <- W.asValue =<< W.getWith opts (urlify resource)
  return $ res ^. W.responseBody

delete ∷ String → IO A.Value
delete resource = do
  res <- W.asValue =<< W.deleteWith opts (urlify resource)
  return $ res ^. W.responseBody

projects ∷ IO [T.Text]
projects = get "project" >>| \v -> v ^.. values . key "name" . _String

createIssue ∷ T.Text → T.Text → T.Text → T.Text → IO T.Text
createIssue proj summary description ty = do
  let obj = A.Object $ M.singleton "fields" $ A.Object $ M.fromList [
              ("project", A.Object $ M.singleton "key" $ A.String proj),
              ("summary", A.String summary),
              ("description", A.String description),
              ("issuetype", A.Object $ M.singleton "name" $ A.String ty)
            ]
  post "issue" obj >>| \v -> v ^. key "key" . _String

commentIssue ∷ T.Text → T.Text → IO ()
commentIssue issue comment = do
  let obj = A.Object $ M.singleton "body" $ A.String comment
  _ <- post ("issue/" ++ T.unpack issue ++ "/comment") obj
  return ()

readIssue ∷ T.Text → IO (T.Text, T.Text, T.Text, T.Text, [(T.Text, T.Text)])
readIssue issue = do
  res <- get ("issue/" ++ T.unpack issue)
  let issueKey         = res ^. key "key" . _String
      issueProj        = res ^. key "fields" . key "project" . key "name" . _String
      issueSummary     = res ^. key "fields" . key "summary" . _String
      issueDescription = res ^. key "fields" . key "description" . _String
      issueCommentA    = res ^.. key "fields" . key "comment" . key "comments" . values . key "author" . key "name" . _String
      issueCommentB    = res ^.. key "fields" . key "comment" . key "comments" . values . key "body" . _String
  return (issueKey, issueProj, issueSummary, issueDescription, zip issueCommentA issueCommentB)

searchIssue ∷ T.Text → IO [(T.Text, T.Text, T.Text)]
searchIssue query = do
  res <- W.asValue =<< W.getWith (opts & W.param "jql" .~ [T.concat ["(Summary ~ \"", query, "\" OR Description ~ \"", query, "\")"]] & W.param "fields" .~ ["key,summary,project"]) (urlify "search")
  let issueNames        = res ^.. W.responseBody . key "issues" . values . key "key" . _String
      issueProjects     = res ^.. W.responseBody . key "issues" . values . key "fields" . key "project" . key "name" . _String
      issueSummaries    = res ^.. W.responseBody . key "issues" . values . key "fields" . key "summary" . _String
  return $ zip3 issueNames issueProjects issueSummaries

deleteIssue ∷ T.Text → IO ()
deleteIssue key = do
  _ <- delete ("issue/" ++ T.unpack key)
  return ()
