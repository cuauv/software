module Utility.Types where

import qualified Data.Text       as T
import qualified Data.Time.Clock as CL
import           GHC.Generics

data Host = Host { hostName :: T.Text, hostPort :: Int } deriving (Show, Eq, Generic)

data Encoding = Encoding {
  encodingFormat      :: Format,
  encodingCompression :: Compression
} deriving (Show, Eq, Generic)

data Format = JSON | Binary deriving (Read, Show, Eq, Generic)

data Compression = None | LZMA | GZIP | ZLIB deriving (Read, Show, Eq, Generic)

data LogLevel = DBG | STD | CRT deriving (Show, Generic, Eq)

data LogCategory =

  INT |
  EXT |
  NET

  deriving (Show, Generic, Eq)

data LogEntry = LogEntry {
  leTimestamp :: CL.UTCTime,
  leLevel     :: LogLevel,
  leCategory  :: LogCategory,
  leEvent     :: T.Text
} deriving (Generic, Eq)
