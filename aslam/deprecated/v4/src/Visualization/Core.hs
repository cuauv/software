module Visualization.Core where

import           Control.Concurrent
import           Control.Monad
import qualified Data.HashMap.Strict as M
import qualified Data.Text           as T
import qualified Data.Vector         as V
import qualified Graphics.UI.GLUT    as GLUT

import           Protocol.DSL
import           Utility.Networking

addr = "tcp://127.0.0.1:8080"

type VState = M.HashMap T.Text (GLUT.Window, ThreadId)

launch env obj = do
  GLUT.initialize "ASLAM" []
  GLUT.initialDisplayMode GLUT.$= [GLUT.DoubleBuffered]
  GLUT.createWindow $ "ASLAM: " ++ T.unpack obj
  GLUT.reshapeCallback GLUT.$= Just (\size -> GLUT.viewport GLUT.$= (GLUT.Position 0 0, size) >> GLUT.postRedisplay Nothing)

  points <- newMVar (V.empty :: V.Vector (GLUT.GLfloat, GLUT.GLfloat, GLUT.GLfloat, GLUT.GLfloat))
  angles <- newMVar (0.0 :: GLUT.GLfloat, 0.0 :: GLUT.GLfloat, 0.0 :: GLUT.GLfloat)
  scale  <- newMVar (1.0 :: GLUT.GLfloat)

  forkIO $ track env obj points

  GLUT.displayCallback GLUT.$= display points angles scale
  GLUT.keyboardMouseCallback GLUT.$= Just (keyboardMouse scale)
  GLUT.idleCallback GLUT.$= Just (GLUT.postRedisplay Nothing)
  GLUT.motionCallback GLUT.$= Just (motion angles)

  GLUT.mainLoop

display points angles scale = do
  points <- readMVar points
  angles <- readMVar angles
  scales <- readMVar scale

  let (xa, ya, za) = angles

  GLUT.loadIdentity
  GLUT.scale scales scales scales

  GLUT.rotate xa $ GLUT.Vector3 1 0 0
  GLUT.rotate ya $ GLUT.Vector3 0 1 0
  GLUT.rotate za $ GLUT.Vector3 0 0 1

  GLUT.clear [GLUT.ColorBuffer]
  GLUT.renderPrimitive GLUT.Points $ mapM_ (\(a, b, c, d) -> constructPoint (a, b, c) (d / 1, 0, 1 / d)) points

  GLUT.swapBuffers

constructPoint (x, y, z) (r, g, b) = do
  GLUT.color $ GLUT.Color3 r g b
  GLUT.vertex $ GLUT.Vertex3 x y z

keyboardMouse scale (GLUT.MouseButton GLUT.WheelUp) _ _ _ = modifyMVar_ scale (return . (*) 1.1)
keyboardMouse scale (GLUT.MouseButton GLUT.WheelDown) _ _ _ = modifyMVar_ scale (return . (*) 0.9)
keyboardMouse _ _ _ _ _ = return ()

motion angles (GLUT.Position mx my) = do
  (GLUT.Size x y) <- GLUT.get GLUT.windowSize
  modifyMVar_ angles (const $ return (360 * (fromIntegral my - (fromIntegral y / 2)) / fromIntegral y, 360 * (fromIntegral mx - (fromIntegral x / 2)) / fromIntegral x, 0))

toGL :: Double -> GLUT.GLfloat
toGL = fromRational . toRational

track env obj points = forever $ do
  res <- call addr (ExecD env $ EvalD (VarE obj) (Sample 1000))
  let Just (RespPhaseSpace (PhaseSpace r)) = res
  modifyMVar_ points (const $ return $ V.map (\(x, y) -> let x' = V.map toGL $ (unlift x :: V.Vector Double) in (x' V.! 0, x' V.! 1, x' V.! 2, 2550 * toGL y)) r)
  -- delay 1.0
