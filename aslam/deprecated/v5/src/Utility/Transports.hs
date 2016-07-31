module Utility.Transports where

import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString.Char8   as B
import qualified Data.ByteString.Lazy    as BL
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Nanomsg                 as N
import qualified Network.WebSockets      as WS
import qualified Network.WebSockets.Snap as WSS
import qualified Snap.Core               as Snap
import qualified Snap.Http.Server        as Snap

import           Utility.Serialization
import           Utility.Types

toNanomsg ∷ Host → String
toNanomsg (Host h p) = "tcp://" ++ T.unpack h ++ ":" ++ show p

serveNanomsg ∷ forall a b . (Serializable a, Serializable b) ⇒ Host → (a → IO b) → IO ()
serveNanomsg h f = do
  let encoding = Encoding JSON None
  N.withSocket N.Rep $ \sock -> do
    _ <- N.bind sock $ toNanomsg h
    forever $ do
      msg <- N.recv sock
      case deserialize encoding $ BL.fromStrict msg of
        Just req -> do
          res <- f req
          N.send sock $ BL.toStrict $ serialize encoding res
        Nothing  -> do
          N.send sock ""

runWSServer ∷ forall a b . (Serializable a, Serializable b) ⇒ Host → (b → IO ()) → IO (a → IO ())
runWSServer h recv = do
  chan <- newChan
  _ <- forkIO $ Snap.httpServe (config h) $ do
    Snap.modifyResponse $ Snap.setHeader "Server" "ASLAM v0.8.0"
    WSS.runWebSocketsSnap $ \pending -> do
      conn <- WS.acceptRequest pending
      copy <- dupChan chan
      runWSThreads conn (readChan copy) recv
  _ <- forkIO $ forever $ void $ readChan chan -- Drain.
  return $ writeChan chan

runWSThreads ∷ forall a b . (Serializable a, Serializable b) ⇒ WS.Connection → IO a → (b → IO ()) → IO ()
runWSThreads conn send recv = do
  _ <- forkIO $ forever $ do
    msg <- send
    WS.sendBinaryData conn $ serialize encoding msg
  forever $ do
    raw <- WS.receiveData conn
    case deserialize encoding raw of
      Just msg -> recv msg
      Nothing  -> return ()

encoding ∷ Encoding
encoding = Encoding JSON ZLIB

config ∷ forall m a . Snap.MonadSnap m ⇒ Host → Snap.Config m a
config (Host h p) =
  Snap.setHostname (T.encodeUtf8 h) $
  Snap.setPort p $
  Snap.setAccessLog (Snap.ConfigIoLog B.putStrLn) $
  Snap.setErrorLog (Snap.ConfigIoLog B.putStrLn) $
  Snap.setCompression True $
  Snap.defaultConfig
