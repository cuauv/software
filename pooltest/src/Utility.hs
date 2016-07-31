module Utility where

import           Control.Concurrent
import qualified Control.Exception     as E
import qualified Data.Text             as T
import qualified Data.Text.IO          as T
import           Data.Time.Clock.POSIX
import qualified System.Console.ANSI   as C

(|<<) ∷ Functor f ⇒ (a → b) → f a → f b
(|<<) = fmap
{-# INLINE (|<<) #-}

(>>|) ∷ Functor f ⇒ f a → (a → b) → f b
(>>|) = flip fmap
{-# INLINE (>>|) #-}

delay ∷ Double → IO ()
delay = threadDelay . round . (*) 1e6
{-# INLINE delay #-}

catchAny ∷ IO a → (E.SomeException → IO a) → IO a
catchAny x y = x `E.catch` (\(e :: E.SomeException) -> y e)
{-# INLINE catchAny #-}

unixTime ∷ IO Double
unixTime = (fromRational . toRational) |<< getPOSIXTime
{-# INLINE unixTime #-}

contLog ∷ T.Text → IO ()
contLog msg = do
  T.putStrLn $ withOverwrite msg

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

withUnderline ∷ T.Text → T.Text
withUnderline = withSGR [C.SetUnderlining C.SingleUnderline]

prettyPrintIssue ∷ T.Text → T.Text → T.Text → T.Text
prettyPrintIssue key proj summary = T.concat [
  withUnderline "NAME",
  " ",
  withBold $ withFColor C.Red key,
  " ",
  withUnderline "PROJECT",
  " ",
  withBold $ withFColor C.Blue proj,
  " ",
  withUnderline "SUMMARY",
  " ",
  summary
  ]

prettyPrintComments ∷ [(T.Text, T.Text)] → T.Text
prettyPrintComments = T.unlines . map (\(author, body) -> T.concat [withFColor C.Green author, ": ", body])
