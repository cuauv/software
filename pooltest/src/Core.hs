module Core where

import           Control.Monad
import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as BL
import           Data.Char
import qualified Data.HashMap.Strict  as M
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import qualified Data.Text.IO         as T
import qualified Data.Time.Clock      as CL
import qualified System.Console.ANSI  as C
import           System.Directory
import           System.Environment
import           System.Exit
import           System.IO
import           System.Posix.Files
import qualified System.Process       as P

import qualified JIRA                 as J
import           Protocol
import qualified SHM
import qualified Slack                as S
import           Utility

handle ∷ Command → IO ()
handle (Start name) = do
  if "/" `T.isInfixOf` name then
    putStrLn "Slashes in pooltest names are not permitted!"
  else do
      sub <- sub
      logDir <- newLogDirectory name
      now <- unixTime
      let metaInfo = A.Object $ M.fromList [
                      ("startTime", A.Number $ fromRational $ toRational now),
                      ("submarine", A.String $ T.pack $ show sub)
                      ]
      BL.writeFile (logDir ++ "/metaStart.json") $ A.encode metaInfo
      launchSHMLog logDir
      S.send sub $ T.concat ["Pooltest has been started. Local logs will be on the submarine at ", T.pack logDir]
handle Assert = do
  (code, _, _) <- P.readProcessWithExitCode "pgrep" ["auv-shmlogd"] ""
  if (code == ExitFailure 1) then do
    let dir = currentDirectorySynlink
    launchSHMLog dir
    sub <- sub
    S.send sub $ T.concat ["shmlog automatically restarted in current pooltest directory: ", T.pack dir]
  else return ()
handle (Log m) = do
  sub <- sub
  let file = concat [currentDirectorySynlink, "/", "log.txt"]
  timestamp <- unixTime
  BL.appendFile file (A.encode $ A.Object $ M.fromList [("message", A.String m), ("timestamp", A.Number $ fromRational $ toRational timestamp)])
  S.send sub $ T.concat ["Manual log entry: ", m]
  return ()
handle (BatteryChange b) = do
  SHM.shm_init
  sub <- sub
  let file = concat [currentDirectorySynlink, "/", "battery_log.txt"]
  timestamp <- CL.getCurrentTime
  let entry = T.concat ["Batteries changed at " , T.pack $ show timestamp, " to ", b, ".\n"]
  T.appendFile file entry
  S.send sub entry
  SHM.battery_status_battery_name SHM.$> T.encodeUtf8 b
  return ()
handle End = do
  sub <- sub
  now <- unixTime
  current <- currentLogDirectory
  let metaInfo = A.Object $ M.singleton "endTime" $ A.Number $ fromRational $ toRational now
  (BL.writeFile (current ++ "/metaEnd.json") $ A.encode metaInfo) `catchAny` (const $ return ())
  let end     = dirEnd current
      remote  = "software@cuauv.org:/srv/logs/pooltest"
      command = "rsync -avzH -e \"ssh -p 2222\" --human-readable --progress \"" ++ current ++ "\" " ++ remote
      logURL  = T.pack $ "https://cuauv.org/log/" ++ end
  P.callCommand "pkill auv-shmlogd" `catchAny` (const $ return ())
  P.callCommand command
  removeLink currentDirectorySynlink `catchAny` (const $ return ())
  let defaultDir = "none"
  createDirectoryIfMissing True $ concat [logDirectory, "/", defaultDir]
  createSymbolicLink defaultDir currentDirectorySynlink
  S.send sub $ T.concat ["Pooltest has been completed. Logs can be accessed at ", logURL]
handle Cleanup = do
  current <- currentLogDirectory
  stored <- filter (\x -> not $ x `elem` [current, "current", ".", ".."]) |<<  getDirectoryContents logDirectory
  flip mapM_ stored $ \dir -> do
    let combined = concat [logDirectory, "/", dir]
    T.putStr $ T.concat ["Would you like to delete directory ", T.pack combined, " [Y/n] ? "]
    hFlush stdout
    str <- T.getLine
    case T.toLower str of
      "y" -> do
        removeDirectoryRecursive combined
        T.putStrLn $ T.concat ["Directory ", T.pack combined, " removed"]
      _   -> do
        return ()
  return ()
handle (Jira j) = do
  case j of
    CreateC (Create proj ty summary description) -> do
      key <- J.createIssue (toProject proj) summary description (toIssueType ty)
      T.putStrLn $ "Issue created. Key: " `T.append` withFColor C.Blue key `T.append` "."
    SearchC (Search query) -> do
      res <- J.searchIssue query
      mapM_ (\(k, p, s) -> T.putStrLn $ prettyPrintIssue k p s) res
    CommentC (Comment key body) -> do
      J.commentIssue key body
      T.putStrLn $ "Commented on issue " `T.append` withFColor C.Blue key `T.append` "."
    ReadC (Read key) -> do
      (k, p, s, d, c) <- J.readIssue key
      T.putStr $ T.unlines [prettyPrintIssue k p s, "", T.concat [withUnderline "DESCRIPTION", " ", d], "", T.concat [withUnderline "COMMENTS", "\n", prettyPrintComments c]]

launchSHMLog ∷ FilePath → IO ()
launchSHMLog dir = do
  timestamp <- CL.getCurrentTime
  P.callCommand $ "auv-shmlogd -o '" ++ dir ++ "/" ++ show timestamp ++ "_shmlog.log' &"

dirEnd ∷ FilePath → FilePath
dirEnd = reverse . takeWhile ((/=) '/') . reverse

toProject ∷ Project → T.Text
toProject Software = "SOF"
toProject Mechanical = "MEC"
toProject Electrical = "ECE"
toProject BusinessPR = "BUS"

toIssueType ∷ IssueType → T.Text
toIssueType Bug = "Bug"
toIssueType Task = "Task"

logDirectory ∷ FilePath
logDirectory = "/var/log/auv"

currentDirectorySynlink ∷ FilePath
currentDirectorySynlink = logDirectory ++ "/current"

currentLogDirectory ∷ IO FilePath
currentLogDirectory = readSymbolicLink currentDirectorySynlink

sub ∷ IO Sub
sub = do
  env <- getEnv "CUAUV_VEHICLE"
  case map toUpper env of
    "THOR" -> return Thor
    "LOKI" -> return Loki
    _      -> return Thor -- Default

newLogDirectory ∷ T.Text → IO FilePath
newLogDirectory name = do
  let directory = concat [logDirectory, "/", T.unpack name]
  createDirectoryIfMissing True directory
  exists <- doesDirectoryExist currentDirectorySynlink
  when exists $ removeLink currentDirectorySynlink
  (P.callCommand "unlink /var/log/auv/current") `catchAny` (const $ return ())
  createSymbolicLink directory currentDirectorySynlink
  return directory
