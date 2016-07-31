module Core.Daemon where

import           Control.Concurrent
import           Control.DeepSeq
import           Control.Monad
import qualified Data.ByteString.Lazy as BL
import           Data.Fixed
import qualified Data.HashMap.Strict  as M
import qualified Data.Text            as T
import qualified Data.Vector.Unboxed  as UV
import           System.Environment

import qualified SHM

import qualified AUVLog
import           Core.API
import           Math
import           Protocol
import           Utility

go ∷ IO ()
go = do

  loggingHandle <- AUVLog.initialize
  let log t m                = AUVLog.wlog loggingHandle ("aslam." `T.append` t) m
      constantFreq freq func = void $ forkIO $ constantFreq' (\m -> log "scheduler.warning" m) freq func

  Config obj nanomsgHost wsHost wsFreq grid4Range grid4Gran grid3Range grid3Gran shmPushFreq shmPullFreq uncerFreq resampleFreq <- do
    root   <- getEnv "CUAUV_SOFTWARE"
    locale <- getEnv "CUAUV_LOCALE"
    conf   <- BL.readFile $ concat [root, "/conf/", locale, ".json"]
    case deserialize configEncoding conf of
      Just c  -> return c
      Nothing -> do
        log "internal.critical" "Configuration deserialization failed; terminating!"
        error "Configuration deserialization failed!"

  log "core" "Configuration loaded successfully."

  let defaultGrid4 ∷ PhaseSpace (Vec4 Double)
      defaultGrid4 = normalize $ grid grid4Gran grid4Range
      defaultGrid3 ∷ PhaseSpace (Vec3 Double)
      defaultGrid3 = normalize $ grid grid3Gran grid3Range

  defaultGrid4 `deepseq` defaultGrid3 `deepseq` SHM.shm_init

  log "core" "Forced initial grid & initialized SHM."

  state <- newMVar M.empty

  let pushSHM3 var ((eX, uX), (eY, uY), (eZ, uZ)) = constantFreq shmPushFreq $ \_ -> do
        withVar var $ \var -> do
          let (x, y, z) = fst $ estimateArgmax id var
              xu = deviance $ mapAcross fst3 var
              yu = deviance $ mapAcross snd3 var
              zu = deviance $ mapAcross thd3 var
          eX SHM.$> x
          eY SHM.$> y
          eZ SHM.$> z
          uX SHM.$> xu
          uY SHM.$> yu
          uZ SHM.$> zu
      pushSHM4 var ((eX, uX), (eY, uY), (eZ, uZ), (eH, uH)) = constantFreq shmPushFreq $ \_ -> do
        withVar var $ \var -> do
          let (x, y, z, h) = fst $ estimateArgmax id var
              xu = deviance $ mapAcross fst4 var
              yu = deviance $ mapAcross snd4 var
              zu = deviance $ mapAcross thd4 var
              hu = deviance $ mapAcross fth4 var
          eX SHM.$> x
          eY SHM.$> y
          eZ SHM.$> z
          eH SHM.$> h
          uX SHM.$> xu
          uY SHM.$> yu
          uZ SHM.$> zu
          uH SHM.$> hu
      pullSHM4 var ((eX, uX), (eY, uY), (eZ, uZ), (eH, uH)) = do
        constantFreq shmPullFreq $ \delta -> do
          disabled <- (SHM.$<) SHM.switches_soft_kill
          unless disabled $ do
            vx <- (SHM.$<) eX
            vy <- (SHM.$<) eY
            vd <- (SHM.$<) eZ
            vh <- (|<<) radians $ (SHM.$<) eH
            hd <- (|<<) radians $ (SHM.$<) eH -- TODO FIXME
            let ch = cos hd
                sh = sin hd
                vn = (ch * vx) - (sh * vy)
                ve = (ch * vy) + (sh * vx)
            updateVar var $ \var -> do
              return $!! mapAcross ((.+) (vn * delta, ve * delta, vd * delta, vh * delta)) var
        constantFreq uncerFreq $ \delta -> do
          uX <- (SHM.$<) uX
          uY <- (SHM.$<) uY
          uZ <- (SHM.$<) uZ
          uH <- (SHM.$<) uH
          let len = grid4Gran ^ (4 :: Int)
          noise <- noiseGen len (uX, uY, uZ, uH)
          updateVar var $ \var -> return $!! UV.zipWith (\(x, p) n -> (x .+ (n .* delta), p)) var noise
      resample4 var freq = constantFreq freq $ const $ do
                              updateVar var $ \var -> return $!! autoresample var
      resample3 var freq = constantFreq freq $ const $ do
                              print freq
                              updateVar var $ \var -> return $!! autoresample var
      addObject name (Object spec maybeResample initialUncertainty) = do
        var <- case spec of
          Vec4 pos maybeOut maybeIn -> do
            var <- newVar defaultGrid4
            updateVar var $ return . normalize . apply (\ !val -> gaussian (val .\\ pos) zero initialUncertainty)
            case maybeOut of
              Just pairs -> pushSHM4 var pairs
              Nothing    -> return ()
            case maybeIn of
              Just pairs -> pullSHM4 var pairs
              Nothing    -> return ()
            case maybeResample of
              Just freq -> resample4 var freq
              Nothing    -> return ()
            return $ Var4 var
          Vec3 pos maybeOut -> do
            var <- newVar defaultGrid3
            updateVar var $ return . normalize . apply (\ !val -> gaussian (val .\\ pos) zero initialUncertainty)
            case maybeOut of
              Just pairs -> pushSHM3 var pairs
              Nothing    -> return ()
            case maybeResample of
              Just freq  -> resample3 var freq
              Nothing    -> return ()
            return $ Var3 var
        modifyMVar_ state (return . M.insert name var)

  mapM_ (uncurry addObject) $ M.toList obj
  log "core" "All objects initialized."

  log "net" "Launching Nanomsg server."
  _ <- forkIO $ serveNanomsg nanomsgHost $ \req -> do
    let fail = return $ RErr "Invalid request."
    state <- readMVar state
    case req of
      ObsCon "heading" [fromObj, toObj] (RealE h) uncer -> do
        case (M.lookup fromObj state, M.lookup toObj state) of
          (Just (Var4 fromVar), Just (Var3 toVar)) -> do
            -- hd <- (|<<) radians $ (SHM.$<) SHM.kalman_heading -- TODO FIXME
            actX <- (SHM.$<) SHM.kalman_north
            actY <- (SHM.$<) SHM.kalman_east
            updateVar toVar $ return . normalize . apply (\ !to -> gaussianH (heading2 (actX, actY) (fst3 to, snd3 to)) h uncer)
            -- updateVar2 fromVar toVar $ return . normalize2 . apply (\ (!from, !to) -> gaussianH (heading2 (fst4 from, snd4 from) (fst3 to, snd3 to)) h uncer)
            return ROK
          _ -> fail
      ObsCon "distance" [fromObj, toObj] (RealE d) uncer -> do
        case (M.lookup fromObj state, M.lookup toObj state) of
          (Just (Var4 fromVar), Just (Var3 toVar)) -> do
            actX <- (SHM.$<) SHM.kalman_north
            actY <- (SHM.$<) SHM.kalman_east
            updateVar toVar $ return . normalize . apply (\ !to -> gaussian (distance2 (actX, actY) (fst3 to, snd3 to)) d uncer)
            -- updateVar2 fromVar toVar $ return . normalize2 . apply (\ (!from, !to) -> gaussian (distance2 (fst4 from, snd4 from) (fst3 to, snd3 to)) d uncer)
            return ROK
          _ -> fail
      _ -> fail
  log "net" $ T.concat ["Nanomsg server launched on tcp://", hostName nanomsgHost, ":", T.pack $ show $ hostPort nanomsgHost]

  log "net" "Launching websocket server."
  broadcast <- runWSServer wsHost (\(_ :: ()) -> return ())
  constantFreq wsFreq $ \_ -> do
    let fil = limit 500
    readMVar state >>= \state -> flip mapM_ (M.toList state) $ \(k, v) -> do
      case v of
        Var4 var -> do
          var <- readVar var
          broadcast $ GUIUpdate k (Space4 $ fil var)
        Var3 var -> do
          var <- readVar var
          broadcast $ GUIUpdate k (Space3 $ fil var)
  log "net" $ T.concat ["Websocket server launched on ws://", hostName wsHost, ":", T.pack $ show $ hostPort wsHost]

  forever $ delay 100.0

constantFreq' ∷ (T.Text → IO ()) → Double → (Double → IO ()) → IO ()
constantFreq' warn freq func = do
  let desired   = 1 / freq
      step prev = do
        start <- unixTime
        let delta = start - prev
        func delta
        end   <- unixTime
        let trem = desired - (end - start)
        if trem >= 0 then delay trem else warn $ "Failed to meet desired frequency. Remaining time: " `T.append` (T.pack $ show trem)
        step start
  step =<< unixTime

updateVar ∷ forall a . NFData a ⇒ MVar a → (a → IO a) → IO ()
updateVar var func =
  modifyMVar_ var $ \val -> do
    res <- func val
    return $!! res

updateVar2 ∷ forall a b . (NFData a, NFData b) ⇒ MVar a → MVar b → ((a, b) → IO (a, b)) → IO ()
updateVar2 varA varB func =
  modifyMVar_ varA $ \valA -> do
    modifyMVar varB $ \valB -> do
      (newA, newB) <- func (valA, valB)
      return $!! (newB, newA)

normalize2 ∷ forall a b . (UV.Unbox a, UV.Unbox b) ⇒ (PhaseSpace a, PhaseSpace b) → (PhaseSpace a, PhaseSpace b)
normalize2 (a, b) = (normalize a, normalize b)

withVar ∷ forall a . Var a → (PhaseSpace a → IO ()) → IO ()
withVar = withMVar

readVar ∷ forall a . Var a → IO (PhaseSpace a)
readVar = readMVar

newVar ∷ forall a . PhaseSpace a → IO (Var a)
newVar = newMVar

forkLoop ∷ IO a → IO ()
forkLoop = void . forkIO . forever

configEncoding ∷ Encoding
configEncoding = Encoding JSON None

radians ∷ Double → Double
radians = (*) (pi / 180)
--when :: forall m . (Monad m) => Bool -> m () -> m ()
--when bool func = if bool then func else return ()
