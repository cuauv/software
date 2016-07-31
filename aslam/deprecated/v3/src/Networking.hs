module Networking where

import Auxiliary

import Control.Monad
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified System.ZMQ4.Monadic as Z
import System.Random

serialize = BL.toStrict . A.encode
deserialize = A.decodeStrict

wrapped f def x =
  case deserialize x of
    Just x -> serialize |<< f x
    Nothing -> serialize |<< def

serveSynchronously address function =
  Z.runZMQ $ do
    sock <- Z.socket Z.Rep
    Z.bind sock address
    forever $ Z.receive sock >>= (Z.liftIO . function) >>= Z.send sock []

serveSynchronouslyConcurrently address nworkers function =
  let proxy addr = do
        front <- Z.socket Z.Router
        Z.bind front address
        back <- Z.socket Z.Dealer
        Z.bind back addr
        Z.proxy front back Nothing
      worker addr = do
        sock <- Z.socket Z.Rep
        Z.connect sock addr
        forever $ Z.receive sock >>= (Z.liftIO . function) >>= Z.send sock []
   in do
    addr <- genAddr
    Z.runZMQ $ (replicateM_ nworkers $ Z.async $ worker addr) >> proxy addr

toZMQ host port = "tcp://" ++ host ++ ":" ++ show port

genAddr = (++) "inproc://" |<< (replicateM 20 $ randomRIO ('a', 'z'))

callSynchronously address string =
  Z.runZMQ $ do
    sock <- Z.socket Z.Req
    Z.connect sock address
    Z.send sock [] string
    Z.receive sock

callW host port cmd = do
  res <- callSynchronously (toZMQ host port) (serialize cmd)
  return $ deserialize res
