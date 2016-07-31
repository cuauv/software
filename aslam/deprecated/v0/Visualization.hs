-- module Visualization where
module Main where

import qualified API
import Networking

import Control.Monad
import Control.Concurrent
import qualified Graphics.UI.GLUT as GLUT
import System.Environment

main :: IO ()
main = do
  name:_ <- getArgs
  (_, _) <- GLUT.getArgsAndInitialize
  GLUT.initialDisplayMode GLUT.$= [GLUT.DoubleBuffered]
  window <- GLUT.createWindow $ "Adaptive SLAM Visualization System"
  GLUT.reshapeCallback GLUT.$= Just reshape
  degree <- newMVar (1.0 :: GLUT.GLfloat)
  angles <- newMVar (0.0, 0.0, 0.0) :: IO (MVar (GLUT.GLfloat, GLUT.GLfloat, GLUT.GLfloat))
  points <- newMVar [] :: IO (MVar [(GLUT.GLfloat, GLUT.GLfloat, GLUT.GLfloat, GLUT.GLfloat)])
  forkIO $ track name points
  GLUT.keyboardMouseCallback GLUT.$= Just (keyboardMouse degree angles)
  GLUT.displayCallback GLUT.$= display degree angles points
  GLUT.idleCallback GLUT.$= Just (idle degree angles)
  GLUT.passiveMotionCallback GLUT.$= Just (motion angles)
  GLUT.mainLoop

keyboardMouse degree angles key GLUT.Down _ _ = case key of
  (GLUT.Char '=') -> GLUT.scale (0.9 :: GLUT.GLfloat) (0.9 :: GLUT.GLfloat) (0.9 :: GLUT.GLfloat)
  (GLUT.Char '-') -> GLUT.scale (1.1 :: GLUT.GLfloat) (1.1 :: GLUT.GLfloat) (1.1 :: GLUT.GLfloat)
  (GLUT.SpecialKey GLUT.KeyLeft) -> GLUT.rotate (-0.5 :: GLUT.GLfloat) $ GLUT.Vector3 0 1 0
  (GLUT.SpecialKey GLUT.KeyRight) -> GLUT.rotate (0.5 :: GLUT.GLfloat) $ GLUT.Vector3 0 1 0
  (GLUT.SpecialKey GLUT.KeyUp) -> GLUT.rotate (-0.5 :: GLUT.GLfloat) $ GLUT.Vector3 1 0 0
  (GLUT.SpecialKey GLUT.KeyDown) -> GLUT.rotate (0.5 :: GLUT.GLfloat) $ GLUT.Vector3 1 0 0
  (GLUT.SpecialKey GLUT.KeyPageUp) -> GLUT.rotate (-0.5 :: GLUT.GLfloat) $ GLUT.Vector3 0 0 1
  (GLUT.SpecialKey GLUT.KeyPageDown) -> GLUT.rotate (0.5 :: GLUT.GLfloat) $ GLUT.Vector3 0 0 1
  _ -> return ()

keyboardMouse _ _ _ _ _ _ = return ()

motion angles (GLUT.Position mx my) = do
  print $ (mx, my)
  modifyMVar_ angles $ \(_, _, z) -> 
    return (sin (fromIntegral mx / 100), cos (fromIntegral my / 100), 0)
  

reshape size = do
  GLUT.viewport GLUT.$= (GLUT.Position 0 0, size)
  GLUT.postRedisplay Nothing

{-
idle degree angle = do
  modifyMVar_ degree (return . (+) 0.1)
  modifyMVar_ angle (return . (+) 0.1)
  GLUT.postRedisplay Nothing
-}

idle degree angle = GLUT.postRedisplay Nothing

display degree angles points = do
  degree <- readMVar degree
  (xa, ya, za) <- readMVar angles
  points <- readMVar points
  GLUT.clear [ GLUT.ColorBuffer ]
  GLUT.rotate za $ GLUT.Vector3 0 0 1
  GLUT.rotate ya $ GLUT.Vector3 0 1 0
  GLUT.rotate xa $ GLUT.Vector3 1 0 0
  -- GLUT.preservingMatrix $ GLUT.renderPrimitive GLUT.Points $ mapM_ (\p@(x, y, z) -> constructPoint (x, y, 0) (z / degree, 0, degree / z)) $ points
  GLUT.renderPrimitive GLUT.Points $ mapM_ (\(a, b, c, d) -> constructPoint (a, b, c) (d / 1, 0, 1 / d)) points
  -- GLUT.flush
  GLUT.swapBuffers

constructPoint (x,y,z) (r,g,b) = do
    GLUT.color $ GLUT.Color3 r g b
    GLUT.vertex $ GLUT.Vertex3 x y z

-- points :: [(GLUT.GLfloat, GLUT.GLfloat, GLUT.GLfloat, GLUT.GLfloat)]
-- points = map (\(x, y, z) -> (xfm x / 100, xfm y / 100, xfm z / 100, (xfm $ P.evaluate pm (x, y, z)) / 100)) [(x, y, z) | x <- [-50 .. 50], y <- [-50 .. 50], z <- [-50 .. 50]]

call :: API.Request -> IO (Maybe API.Response)
call = callW (toZMQ "127.0.0.1" 6666) 10.0

xfm = fromRational . toRational 

track n points = forever $ do
  res <- call $ API.Sample n
  case res of
    Just (API.Sampled p) -> do
      print p
      modifyMVar_ points $ \_ -> return (map (\(x, y, z, v) -> (xfm x, xfm y, xfm z, (200 ** 3) * xfm v / 5)) p)
    _ -> putStrLn "Error contacting ASLAM daemon."
  threadDelay $ round 1e6

-- points :: [(GLUT.GLfloat, GLUT.GLfloat, GLUT.GLfloat)]
-- points = [(sin(2*pi*k/120), cos(2*pi*k/120), -sin(2 * pi * k / 120)) | k <- [1..120]]
