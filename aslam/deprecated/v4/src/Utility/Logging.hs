module Utility.Logging where

import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Data.Time.Clock     as CL
import qualified Data.Time.Format    as CF
import           GHC.Generics
import qualified System.Console.ANSI as C
import           System.IO

data LogLevel = DBG | STD | CRT deriving (Show, Generic, Eq)

data LogCategory = ENV | JIT | NET deriving (Show, Generic, Eq)

data LogEntry = LogEntry {
  leTimestamp :: CL.UTCTime,
  leLevel     :: LogLevel,
  leCategory  :: LogCategory,
  leEvent     :: T.Text
} deriving (Show, Generic, Eq)

disp :: LogLevel -> T.Text
disp DBG = "?"
disp STD = "●"
disp CRT = "×"

colorize :: LogLevel -> C.Color
colorize DBG = C.Green
colorize STD = C.Yellow
colorize CRT = C.Red

colorizeCat :: LogCategory -> C.Color
colorizeCat ENV = C.Magenta
colorizeCat JIT = C.Cyan
colorizeCat NET = C.Blue

formatTime :: CF.FormatTime t => t -> T.Text
formatTime = T.pack . CF.formatTime CF.defaultTimeLocale "%Y/%m/%d %T UTC"

writeOut :: Handle -> LogEntry -> IO ()
writeOut handle (LogEntry t l c e) =
  T.hPutStr handle $ T.concat [
    "[",
    withBold $ withFColor (colorize l) (disp l),
    "] ",
    withBold $ withUnderline $ withDColor (colorizeCat c) $ T.pack $ show c,
    " ",
    withBold $ formatTime t,
    " ",
    e,
    "\n"
    ]

simpleLog :: LogLevel -> LogCategory -> T.Text -> IO ()
simpleLog x y z = do
  time <- CL.getCurrentTime
  writeOut stderr $ LogEntry time x y z

contLog :: T.Text -> IO ()
contLog x = do
  T.hPutStr stderr (withOverwrite x)

withOverwrite :: T.Text -> T.Text
withOverwrite x = (T.pack $ C.setCursorColumnCode 0) `T.append` (T.pack $ C.clearFromCursorToLineEndCode) `T.append` x

withSGR :: [C.SGR] -> T.Text -> T.Text
withSGR x y = (T.pack $ C.setSGRCode x) `T.append` y `T.append` (T.pack $ C.setSGRCode [C.Reset])

withFColor :: C.Color -> T.Text -> T.Text
withFColor c = withSGR [C.SetColor C.Foreground C.Vivid c]

withDColor :: C.Color -> T.Text -> T.Text
withDColor c = withSGR [C.SetColor C.Foreground C.Dull c]

withBold :: T.Text -> T.Text
withBold = withSGR [C.SetConsoleIntensity C.BoldIntensity]

withUnderline :: T.Text -> T.Text
withUnderline = withSGR [C.SetUnderlining C.SingleUnderline]
