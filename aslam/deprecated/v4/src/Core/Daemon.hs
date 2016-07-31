module Core.Daemon where

import           Control.Concurrent
import qualified Data.HashMap.Strict   as M
import qualified Data.Text             as T

import           Execution.Interpreter
import           Protocol.DSL
import           Utility.Control
import           Utility.Logging
import           Utility.Networking

addr = "tcp://0.0.0.0:9191"

main = do
  state <- newMVar M.empty
  simpleLog STD NET $ "Bound to " `T.append` addr
  serve addr $ \(Call env decl) -> do
    ref <-
      modifyMVar state $ \s -> do
        case M.lookup env s of
          Just ref -> return (s, ref)
          Nothing  -> do
            simpleLog STD ENV $ "Created environment \"" `T.append` env `T.append` "\" on demand."
            nref <- newMVar newState
            return (M.insert env nref s, nref)
    res <- modifyMVar ref (flip update decl)
    return res

    `catchAny` (\e -> simpleLog CRT ENV ("ASLAM internal error - this is probably a bug. Trace: " `T.append` (T.pack $ show e)) >> return (RespError "INTERNAL ERROR"))
