module Protocol where

import qualified Data.Text as T

data Command =

  Jira JiraCmd |
  Start T.Text |
  Log T.Text |
  BatteryChange T.Text |
  End |
  Assert |
  Cleanup |
  Replay

  deriving (Show, Eq)

data JiraCmd =

  CreateC CreateCmd |
  SearchC SearchCmd |
  CommentC CommentCmd |
  ReadC ReadCmd

  deriving (Show, Eq)

data CreateCmd =

  Create Project IssueType T.Text T.Text

  deriving (Show, Eq)

data IssueType =

  Bug |
  Task

  deriving (Show, Eq)

data Project =

  Software |
  Electrical |
  Mechanical |
  BusinessPR

  deriving (Show, Eq)

data CommentCmd =

  Comment T.Text T.Text

  deriving (Show, Eq)

data SearchCmd =

  Search T.Text

  deriving (Show, Eq)

data ReadCmd =

  Read T.Text

  deriving (Show, Eq)

data Sub =

  Thor |
  Loki

  deriving (Show, Eq)
