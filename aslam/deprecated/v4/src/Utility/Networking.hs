module Utility.Networking where

import Control.Monad
import qualified Nanomsg as N
import qualified Data.ByteString.Lazy as BL

import Utility.Serialization

encoding = Encoding JSON None

call h req = 
  N.withSocket N.Req $ \sock -> do
    N.connect sock h
    let msg = BL.toStrict $ serialize encoding req
    N.send sock msg
    res <- N.recv sock
    return $ deserialize encoding $ BL.fromStrict res

serve h fun = 
  N.withSocket N.Rep $ \sock -> do
    N.bind sock h
    forever $ do
      msg <- N.recv sock
      case deserialize encoding $ BL.fromStrict msg of
        Just req -> do
          res <- fun req
          N.send sock $ BL.toStrict $ serialize encoding res
        Nothing  -> do
          N.send sock ""
