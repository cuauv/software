module Main where

import API
import Networking
import Protocol

import Control.Concurrent
import qualified Data.HashMap.Strict as M

main = do
  s <- newMVar M.empty
  serveSynchronously (toZMQ "*" 5555) (wrapped (handle s) (return $ Error "Invalid JSON."))
