module Utility.Logging (writeOut, logNow, contLog, simpleLog) where

import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Data.Time.Clock     as CL
import qualified Data.Time.Format    as CF
import qualified System.Console.ANSI as C
import           System.IO

import           Utility.Types

disp ∷ LogLevel → T.Text
disp DBG = "?"
disp STD = "●"
disp CRT = "×"

colorize ∷ LogLevel → C.Color
colorize DBG = C.Green
colorize STD = C.Yellow
colorize CRT = C.Red

colorizeCat ∷ LogCategory → C.Color
colorizeCat INT = C.Magenta
colorizeCat EXT = C.Cyan
colorizeCat NET = C.Blue

formatTime ∷ CF.FormatTime t ⇒ t → T.Text
formatTime = T.pack . CF.formatTime CF.defaultTimeLocale "%Y/%m/%d %T UTC"

writeOut ∷ Handle → LogEntry → IO ()
writeOut handle (LogEntry t l c e) =
  T.hPutStr handle $ T.concat [
    "[",
    withBold $ withFColor (colorize l) (disp l),
    "] ",
    withBold $ withDColor (colorizeCat c) $ T.pack $ show c,
    " ",
    withBold $ formatTime t,
    " ",
    e,
    "\n"
    ]

simpleLog ∷ LogLevel → LogCategory → T.Text → IO ()
simpleLog x y z = writeOut stderr =<< logNow x y z

logNow ∷ LogLevel → LogCategory → T.Text → IO LogEntry
logNow x y z = do
  time <- CL.getCurrentTime
  return $ LogEntry time x y z

contLog ∷ T.Text → IO ()
contLog x = do
  T.hPutStr stderr (withOverwrite x)

withOverwrite ∷ T.Text → T.Text
withOverwrite x = (T.pack $ C.setCursorColumnCode 0) `T.append` (T.pack $ C.clearFromCursorToLineEndCode) `T.append` x

withSGR ∷ [C.SGR] → T.Text → T.Text
withSGR x y = (T.pack $ C.setSGRCode x) `T.append` y `T.append` (T.pack $ C.setSGRCode [C.Reset])

withFColor ∷ C.Color → T.Text → T.Text
withFColor c = withSGR [C.SetColor C.Foreground C.Vivid c]

withDColor ∷ C.Color → T.Text → T.Text
withDColor c = withSGR [C.SetColor C.Foreground C.Dull c]

withBold ∷ T.Text → T.Text
withBold = withSGR [C.SetConsoleIntensity C.BoldIntensity]
