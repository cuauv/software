module Main where

import Protocol
import Networking
import Symbolic.Syntax

import Control.Monad
import Control.Concurrent
import Data.List
import qualified Graphics.UI.GLUT as GLUT
import System.Environment

main :: IO ()
main = do
  namespace:name:_ <- getArgs
  _ <- GLUT.getArgsAndInitialize
  GLUT.initialDisplayMode GLUT.$= [GLUT.DoubleBuffered]
  window <- GLUT.createWindow $ "Adaptive SLAM Visualization System"
  GLUT.reshapeCallback GLUT.$= Just reshape

  points <- newMVar [] :: IO (MVar [(GLUT.GLfloat, GLUT.GLfloat, GLUT.GLfloat, GLUT.GLfloat)])
  angles <- newMVar (0.0 :: GLUT.GLfloat, 0.0 :: GLUT.GLfloat, 0.0 :: GLUT.GLfloat)
  scale <- newMVar (0.1 :: GLUT.GLfloat)
  
  forkIO $ track namespace name points

  GLUT.displayCallback GLUT.$= display points angles scale
  GLUT.keyboardMouseCallback GLUT.$= Just (keyboardMouse scale)
  GLUT.idleCallback GLUT.$= (Just $ GLUT.postRedisplay Nothing)
  GLUT.motionCallback GLUT.$= (Just $ motion angles)
  GLUT.mainLoop

reshape size = do
  GLUT.viewport GLUT.$= (GLUT.Position 0 0, size)
  GLUT.postRedisplay Nothing

motion angles (GLUT.Position mx my) = do
  (GLUT.Size x y) <- GLUT.get GLUT.windowSize
  modifyMVar_ angles $ \_ -> return (360 * (fromIntegral my - (fromIntegral y/2)) / fromIntegral y, 360 * (fromIntegral mx - (fromIntegral x / 2)) / fromIntegral x, 0)

keyboardMouse _ key GLUT.Down _ _ = case key of
  (GLUT.Char '=') -> GLUT.scale (0.9 :: GLUT.GLfloat) (0.9 :: GLUT.GLfloat) (0.9 :: GLUT.GLfloat)
  (GLUT.Char '-') -> GLUT.scale (1.1 :: GLUT.GLfloat) (1.1 :: GLUT.GLfloat) (1.1 :: GLUT.GLfloat)
  (GLUT.SpecialKey GLUT.KeyLeft) -> GLUT.rotate (-0.5 :: GLUT.GLfloat) $ GLUT.Vector3 0 1 0
  (GLUT.SpecialKey GLUT.KeyRight) -> GLUT.rotate (0.5 :: GLUT.GLfloat) $ GLUT.Vector3 0 1 0
  (GLUT.SpecialKey GLUT.KeyUp) -> GLUT.rotate (-0.5 :: GLUT.GLfloat) $ GLUT.Vector3 1 0 0
  (GLUT.SpecialKey GLUT.KeyDown) -> GLUT.rotate (0.5 :: GLUT.GLfloat) $ GLUT.Vector3 1 0 0
  (GLUT.SpecialKey GLUT.KeyPageUp) -> GLUT.rotate (-0.5 :: GLUT.GLfloat) $ GLUT.Vector3 0 0 1
  (GLUT.SpecialKey GLUT.KeyPageDown) -> GLUT.rotate (0.5 :: GLUT.GLfloat) $ GLUT.Vector3 0 0 1
  _ -> return ()

keyboardMouse scale (GLUT.MouseButton GLUT.WheelUp) _ _ _ = 
  modifyMVar_ scale $ \x -> return (x * 1.1)

keyboardMouse scale (GLUT.MouseButton GLUT.WheelDown) _ _ _ =
  modifyMVar_ scale $ \x -> return (x * 0.9)

keyboardMouse _ _ _ _ _ = return ()

f4 (x, _, _, _) = x
s4 (_, x, _, _) = x
t4 (_, _, x, _) = x
l4 (_, _, _, x) = x

display points angles scale = readMVar points >>= \points -> readMVar angles >>= \angles -> readMVar scale >>= \scale -> 
  let (xa, ya, za) = angles
      xs = map f4 points
      ys = map s4 points
      zs = map t4 points
      (xmin, xmax) = (minimum xs, maximum xs)
      (ymin, ymax) = (minimum ys, maximum ys)
      (zmin, zmax) = (minimum zs, maximum zs)
      c1 = (xmin, ymin, zmin)
      c2 = (ymax, ymax, zmax) in do

  GLUT.loadIdentity

  GLUT.scale scale scale scale

  GLUT.rotate xa $ GLUT.Vector3 1 0 0
  GLUT.rotate ya $ GLUT.Vector3 0 1 0
  GLUT.rotate za $ GLUT.Vector3 0 0 1

  GLUT.clear [GLUT.ColorBuffer]
  GLUT.renderPrimitive GLUT.Points $ mapM_ (\(a, b, c, d) -> constructPoint (a, b, c) (d / 1, 0, 1 / d)) points
  
  unless (length points == 0) $ do
    GLUT.color $ GLUT.Color3 (255 :: GLUT.GLfloat) (255 :: GLUT.GLfloat) (255 :: GLUT.GLfloat)
    GLUT.currentRasterPosition GLUT.$= GLUT.Vertex4 xmin ymin zmin 1
    GLUT.renderString GLUT.TimesRoman10 $ show c1
    GLUT.currentRasterPosition GLUT.$= GLUT.Vertex4 xmax ymax zmax 1
    GLUT.renderString GLUT.TimesRoman10 $ show c2
  
  GLUT.swapBuffers
  where constructPoint (x, y, z) (r, g, b) = do
          GLUT.color $ GLUT.Color3 r g b
          GLUT.vertex $ GLUT.Vertex3 x y z

call = callW "127.0.0.1" 5555

trans (RealR x) = (x, 0, 0)
trans (VectorR [RealR x]) = (x, 0, 0)
trans (VectorR [RealR x, RealR y]) = (x, y, 0)
trans (VectorR [RealR x, RealR y, RealR z]) = (x, y, z)

toGL = fromRational . toRational

track namespace name points = forever $ do  
  res <- call $ Request Nothing namespace $ Sample name
  case res of
    Just (Sampled d) -> do
      print $ maximumBy (\(_, x) (_, y) -> x `compare` y) d
      modifyMVar_ points $ \_ -> return $ map (\(r, p) -> let (x, y, z) = trans r in (toGL x, toGL y, toGL z, toGL p * 255 * 1e14)) d
    _ -> 
      putStrLn $ "Error: " ++ show res
  threadDelay $ round 1e6
