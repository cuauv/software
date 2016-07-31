import qualified Data.Text           as T
import           Options.Applicative

import           Core
import           Protocol
import           Utility

main ∷ IO ()
main = execParser parser >>= handle

parser ∷ ParserInfo Command
parser = info (helper <*> cmd) (fullDesc <> progDesc "" <> header "Pool Test Scripts")

cmd ∷ Parser Command
cmd =
  subparser (
    command "jira" (info (helper <*> jiraOptions) (fullDesc <> progDesc "Manipulate JIRA via REST API")) <>
    command "start" (info (helper <*> startOptions) (fullDesc <> progDesc "Start a new pooltest")) <>
    command "log" (info (helper <*> logOptions) (fullDesc <> progDesc "Log a message to the pooltest log")) <>
    command "battery-change" (info (helper <*> batteryChangeOptions) (fullDesc <> progDesc "Log a battery change (specify batteries in order port,starboard).")) <>
    command "end" (info (helper <*> endOptions) (fullDesc <> progDesc "End the current pooltest & push all logs")) <>
    command "assert" (info (helper <*> assertOptions) (fullDesc <> progDesc "Make sure a SHM log is running")) <>
    command "cleanup" (info (helper <*> cleanupOptions) (fullDesc <> progDesc "Clean up old pooltests"))
  )

assertOptions ∷ Parser Command
assertOptions = pure Assert

startOptions ∷ Parser Command
startOptions = Start <$> (T.pack |<< argument str (metavar "NAME"))

logOptions ∷ Parser Command
logOptions = Log <$> (T.pack |<< argument str (metavar "MESSAGE"))

batteryChangeOptions ∷ Parser Command
batteryChangeOptions = BatteryChange <$> (T.pack |<< argument str (metavar "BATTERY NAME"))

endOptions ∷ Parser Command
endOptions = pure End

cleanupOptions ∷ Parser Command
cleanupOptions = pure Cleanup

jiraOptions ∷ Parser Command
jiraOptions = Jira <$>
  subparser (
    command "create" (info (helper <*> createOptions) (fullDesc <> progDesc "Create a new issue")) <>
    command "search" (info (helper <*> searchOptions) (fullDesc <> progDesc "Search current issues")) <>
    command "comment" (info (helper <*> commentOptions) (fullDesc <> progDesc "Comment on an issue")) <>
    command "read" (info (helper <*> readOptions) (fullDesc <> progDesc "Read information about an issue"))
  )

createOptions ∷ Parser JiraCmd
createOptions = CreateC |<< (Create <$>
  argument (str >>= parseProject) (metavar "PROJECT") <*>
  argument (str >>= parseIssueType) (metavar "TYPE") <*>
  (T.pack |<< argument str (metavar "SUMMARY")) <*>
  (T.pack |<< argument str (metavar "DESCRIPTION")))

searchOptions ∷ Parser JiraCmd
searchOptions = (SearchC . Search) <$>
  (T.pack |<< argument str (metavar "QUERY"))

commentOptions ∷ Parser JiraCmd
commentOptions = CommentC |<< (Comment <$>
  (T.pack |<< argument str (metavar "ISSUE")) <*>
  (T.pack |<< argument str (metavar "BODY")))

readOptions ∷ Parser JiraCmd
readOptions = (ReadC . Read) <$>
  (T.pack |<< argument str (metavar "KEY"))

parseProject ∷ String → ReadM Project
parseProject "software" = return Software
parseProject "electrical" = return Electrical
parseProject "mechanical" = return Mechanical
parseProject "businesspr" = return BusinessPR
parseProject _ = readerError "Unable to parse project!"

parseIssueType ∷ String → ReadM IssueType
parseIssueType "bug" = return Bug
parseIssueType "task" = return Task
parseIssueType _ = readerError "Unable to parse issue type!"
