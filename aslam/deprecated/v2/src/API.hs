module Main where

import Auxiliary
import HaskellImpl
import Protocol

import LoadTemplates

import Control.Concurrent
import Control.Monad
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as M
import qualified Data.ByteString.Lazy as BL
import qualified System.ZMQ4.Monadic as Z
import System.Random (randomRIO)

type State = M.HashMap String (CompiledModel, RuntimeState)

handle :: MVar State -> Request -> IO Response
handle state (WithSchema s (Load m)) = 
  case compileModel m of
    Right cm -> do
      st <- loadModel cm
      modifyMVar_ state (return . M.insert s (cm, st))
      return Success
    Left e -> return $ Error e
handle state (WithSchema s (Observe o)) = do
  poss <- M.lookup s |<< readMVar state
  case poss of
    Just (cm, st) -> do
      res <- observe cm st o
      case res of
        Right () -> return Success
        Left e -> return $ Error e
    Nothing -> return $ Error "Schema not found."
handle state (WithSchema s (Estimate o)) = do
  poss <- M.lookup s |<< readMVar state
  case poss of
    Just (cm, st) -> do
      est <- estimate cm st o
      case est of
        Right r -> return $ Estimated r
        Left e -> return $ Error e
    Nothing -> return $ Error "Schema not found."

main = do
  state <- newMVar M.empty
  serveSynchronously (toZMQ "*" 6666) (wrapped (handle state) (return $ Error "Invalid JSON."))

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
