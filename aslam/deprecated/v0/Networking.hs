module Networking where

import Auxiliary

import Control.Monad
import Data.Restricted
import System.Random
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified System.ZMQ4.Monadic as Z

toZMQ host port = "tcp://" ++ host ++ ":" ++ show port

genAddr = (++) "inproc://" |<< (replicateM 20 $ randomRIO ('a', 'z'))

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

callSynchronously address timeout string =
  Z.runZMQ $ do
    sock <- Z.socket Z.Req
    Z.connect sock address
    Z.setLinger (restrict (0 :: Int)) sock
    Z.send sock [] string
    [es] <- Z.poll (round $ 1e3 * timeout) [Z.Sock sock [Z.In] Nothing]
    if Z.In `elem` es then Just |<< Z.receive sock else return Nothing


serialize = BL.toStrict . A.encode
deserialize = A.decodeStrict

wrapped f def x =
  case deserialize x of
    Just x -> serialize |<< f x
    Nothing -> serialize |<< def

callW addr timeout cmd = do
  res <- callSynchronously addr timeout (serialize cmd)
  return $ deserialize =<< res

