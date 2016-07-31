module Core.REPL where

import           Control.Monad.IO.Class
import qualified Data.Text                as T
import qualified System.Console.ANSI      as C
import           System.Console.Haskeline

import qualified Backends.CodeGen         as LLVM
import           External.Parsing
import           Utility.Logging
import           Utility.Networking

addr = "tcp://127.0.0.1:9191"

out = outputStrLn . T.unpack

loop = do
  input <- getInputLine (T.unpack prompt)
  case input of
    Nothing -> return ()
    Just st -> do
      case parseDecl st of
        Right dc -> do
          out $ withFColor C.Cyan $ T.pack $ show dc
          liftIO $ LLVM.test dc
          {-
          res <- liftIO $ call addr dc
          case res of
            Just RespSuccess -> return ()
            Just (RespError e) -> outputStrLn $ withFColor C.Yellow $ T.unpack e
            Just (RespExpr ex) -> outputStrLn $ show ex
            _ -> return ()
          -}
        Left err -> do
          out $ withFColor C.Yellow $ T.pack $ show err

      loop

main = runInputT defaultSettings loop

prompt = withFColor C.Green $ "ASLAM" `T.append` " " `T.append` withFColor C.Red ">" `T.append` " "
