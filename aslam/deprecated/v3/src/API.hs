module API where

import Auxiliary
import Protocol
import qualified Symbolic.CPPGen as C

import Control.Concurrent
import qualified Data.HashMap.Strict as M

type State = M.HashMap String (Model, C.CompiledModel)

handle :: MVar State -> Request -> IO Response

{-
handle s (Request m n (Reset o)) = do
  s' <- readMVar s
  case M.lookup n s' of
    Just (m', c) | m' == m -> do
      undefined -- reset c
      return Success
    Just _ -> if not o then return $ Error "Models do not match." else do
      res <- C.compile m
      case res of
        Right c -> do
          modifyMVar_ s (return . M.insert n (m, c))
          return Success
        Left l -> return $ Error l
-}

handle s (Request m n a) = do
  s' <- readMVar s
  case M.lookup n s' of
    Just (m', c) | (case m of Just m -> m' == m; Nothing -> True) -> C.handle c a
    Just _ -> return $ Error "Models do not match."
    Nothing -> 
      case m of
        Just m -> do
          res <- C.compile m
          case res of
            Right c -> do
              modifyMVar_ s (return . M.insert n (m, c))
              forkIO $ C.temporal c
              C.handle c a
            Left l -> return $ Error l
        Nothing -> return $ Error "Model not found."
