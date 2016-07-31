module Auxiliary where

import Prelude hiding (log)
import Data.Time.Clock
import Data.Time.Format
import qualified System.Console.ANSI as C
import qualified Data.Aeson.TH as A
import Data.Char
import qualified System.Process as P
import qualified Data.ByteString.Char8 as B
import System.IO.Temp

run cmd = do
  log $ "Invoking \"" ++ cmd ++ "\"."
  (_, _, _, h) <- P.createProcess $ (P.shell cmd)
  P.waitForProcess h

setFC c = C.setSGRCode [C.SetColor C.Foreground C.Vivid c]

withColor c s = (setFC c) ++ s ++ (C.setSGRCode [C.Reset])

log' t s = getCurrentTime >>= \n -> putStrLn ("[" ++ withColor C.Blue (formatTime defaultTimeLocale "%Y:%m:%d %H:%M:%S" n) ++ "] (" ++ t ++ ") " ++ s)

log = log' (withColor C.Green "INFO")

defaultJSONOptions = A.defaultOptions {
  A.fieldLabelModifier = map toLower . dropWhile isLower, -- e.g. <datatype><Fieldname> as record accessor
  A.constructorTagModifier = map toLower,
  A.omitNothingFields = True,
  A.sumEncoding = A.ObjectWithSingleField
}

(>>|) x y = fmap y x
(|<<) x y = fmap x y

deriveSerialization = A.deriveJSON defaultJSONOptions

