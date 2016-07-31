module Core.CLI where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy     as BL
import qualified Data.HashMap.Strict      as M
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Network.WebSockets       as WS
import qualified Network.WebSockets.Snap  as WSS
import           Options.Applicative
import qualified Snap.Http.Server         as Snap
import qualified System.Console.ANSI      as C
import           System.Console.Haskeline hiding (handle)

import qualified Backends.Interpreter     as Interpreter
import qualified Backends.LLVMJIT         as LLVMJIT
import           External.Parsing
import           Protocol.DSL
import           Utility.Control
import           Utility.Logging
import           Utility.Networking
import           Utility.Serialization
import qualified Visualization.Core       as Visualization

data GlobalOptions = GlobalOptions {
  cmd     :: Command,
  host    :: String,
  port    :: Int,
  verbose :: Bool
} deriving (Show)

data Command = Visualize String String | REPL Bool | Daemon deriving (Show)

parser = info (helper <*> globals) (fullDesc <> progDesc "Adaptive SLAM" <> header "Adaptive SLAM v?")

globals :: Parser GlobalOptions
globals = GlobalOptions
  <$> subparser (command "repl" (info replOptions (progDesc "Launch a read-eval-print loop")) <> command "daemon" (info daemonOptions (progDesc "Launch as daemon")) <> command "visualize" (info visualizeOptions (progDesc "Launch the visualizer")))
  <*> strOption (short 'h' <> long "host" <> metavar "HOST" <> help "Host to bind/connect" <> value "127.0.0.1")
  <*> option auto (short 'p' <> long "port" <> metavar "PORT" <> help "Port to bind/connect" <> value 8080)
  <*> switch (long "verbose" <> short 'v' <> help "Log extensive debug output")

replOptions :: Parser Command
replOptions = REPL <$> switch (short 'r' <> long "remote" <> help "Launch remote REPL")

daemonOptions :: Parser Command
daemonOptions = pure Daemon

visualizeOptions :: Parser Command
visualizeOptions = Visualize <$> strOption (short 'e' <> long "env" <> metavar "ENVIRONMENT" <> help "Environment" <> value "default") <*> strOption (short 'o' <> long "object" <> metavar "OBJECT" <> help "Object to visualize")

main = execParser parser >>= run

config = Snap.setHostname "0.0.0.0" $
         Snap.setPort 9090 $
         Snap.setAccessLog (Snap.ConfigIoLog (\m -> simpleLog DBG NET (T.decodeUtf8 m))) $
         Snap.setErrorLog (Snap.ConfigIoLog (\m -> simpleLog CRT NET (T.decodeUtf8 m))) $
         Snap.defaultConfig

run :: GlobalOptions -> IO ()
run opts@(GlobalOptions cmd host port verbose) = do
  let log x y z = if verbose then simpleLog x y z else return ()
      addr = "tcp://" `T.append` (T.pack host) `T.append` ":" `T.append` (T.pack $ show port)
  log DBG ENV ("Initialized with options: " `T.append` (T.pack $ show opts))
  case cmd of
    Visualize env obj ->
      Visualization.launch (T.pack env) (T.pack obj)
    REPL True -> do
      log STD NET $ "Connecting to " `T.append` addr
      repl log (call (T.unpack addr))
    REPL False -> do
      s <- newState
      repl log (\req -> Just |<< handle s log req)
    Daemon -> do
      s <- newState
      log STD NET $ "Binding to " `T.append` addr
      forkIO $ serve (T.unpack addr) (handle s log)
      Snap.httpServe config $
        WSS.runWebSocketsSnap $
          \pending -> do
            conn <- WS.acceptRequest pending
            let encoding = Encoding JSON ZLIB
                send = WS.sendBinaryData conn . serialize encoding
                recv = WS.receiveData conn >>| deserialize encoding
            forever $ do
              req <- recv
              case req of
                Just req -> do
                  res <- handle s log req
                  send res
                Nothing -> return ()

newState :: IO (MVar State)
newState = newMVar M.empty

handle :: MVar State -> (LogLevel -> LogCategory -> T.Text -> IO ()) -> Call -> IO Resp
handle s log (MkEnv name backend) = do
  modifyMVar_ s $ \s -> do
    new <- runBackendT $ case backend of
              Interpreted -> Interpreter.interpreter
              LLVMJIT     -> LLVMJIT.llvmJIT
    return $ M.insert name new s
  return RespSuccess
handle s log (RmEnv name) = do
  modifyMVar_ s $ \s -> do
    case M.lookup name s of
      Just (_, killFunc) -> killFunc
      Nothing -> return ()
    return $ M.delete name s
  return RespSuccess
handle s log (ExecD env decl) = do
  readMVar s >>= \s ->
    case M.lookup env s of
      Just (handleFunc, _) -> handleFunc decl
      Nothing -> return $ RespError "Environment not found."

repl :: (LogLevel -> LogCategory -> T.Text -> IO ()) -> (Call -> IO (Maybe Resp)) -> IO ()
repl log apply = do
  rState <- newMVar "default"
  let loop = do
        input <- getInputLine prompt
        case input of
          Nothing -> do
            liftIO $ log CRT ENV "Terminating"
            return ()
          Just st -> do
            call <- case st of
              ':':'i':' ':env -> return $ Right $ MkEnv (T.pack env) LLVMJIT
              ':':'s':' ':env -> do
                liftIO $ modifyMVar_ rState (const $ return $ T.pack env)
                return $ Left $ "Swapped to environment " `T.append` (T.pack env)
              ':':'d':' ':env -> return $ Right $ RmEnv (T.pack env)
              _ -> do
                    env <- liftIO $ readMVar rState
                    case parseDecl st of
                      Right dc -> return $ Right (ExecD env dc)
                      Left err -> return $ Left $ T.pack $ show err
            case call of
              Right c -> do
                liftIO $ log DBG ENV ("Parsed: " `T.append` (T.decodeUtf8 $ BL.toStrict $ serialize (Encoding JSON None) c))
                res <- liftIO $ apply c
                case res of
                  Just RespSuccess -> return ()
                  Just (RespError e) -> outputStrLn $ T.unpack $ withFColor C.Yellow e
                  Just (RespExpr ex) -> outputStrLn $ show ex
                  Nothing -> liftIO $ log CRT NET "Remote error: undecodable"
              Left err -> outputStrLn $ T.unpack $ withFColor C.Yellow $ err
            loop
  runInputT defaultSettings loop

prompt = T.unpack $ withFColor C.Green $ "ASLAM" `T.append` " " `T.append` withFColor C.Red ">" `T.append` " "

type State = M.HashMap T.Text (Decl -> IO Resp, IO ())
