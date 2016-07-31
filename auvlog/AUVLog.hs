{-# LANGUAGE CPP, OverloadedStrings, TemplateHaskell #-}

module AUVLog where

import           Control.Concurrent
import qualified Data.Aeson            as A
import qualified Data.ByteString.Lazy  as BL
import qualified Data.HashMap.Strict   as M
import qualified Data.Text             as T
import qualified Data.Time.Clock.POSIX as CL
import qualified Nanomsg               as N
import           PseudoMacros

newtype AUVLogConn = AUVLogConn { unAUVLogConn :: N.Socket N.Pub }

initialize ∷ IO AUVLogConn
initialize = do
  sock <- N.socket N.Pub
  _ <- N.connect sock "tcp://127.0.0.1:7654"
  threadDelay 10000
  return $ AUVLogConn sock

logRaw ∷ AUVLogConn → T.Text → T.Text → T.Text → Int → T.Text → T.Text → IO ()
logRaw (AUVLogConn sock) tree msg file line block linetxt = do
  now <- CL.getPOSIXTime
  let obj = A.Object $ M.fromList [
              ("tree", A.String tree),
              ("timestamp", A.Number $ fromRational $ toRational now),
              ("message", A.String msg),
              ("filename", A.String file),
              ("lineno", A.Number $ fromIntegral line),
              ("block", A.String block),
              ("linetxt", A.String linetxt)
              ]
  N.send sock $ BL.toStrict $ A.encode obj

wlog ∷ AUVLogConn → T.Text → T.Text → IO ()
wlog c t m = logRaw c t m __FILE__ __LINE__ ($__PACKAGE__) ($__MODULE__)
