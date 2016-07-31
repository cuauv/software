-- ASLAM Visualization
-- Christopher Goes, 2014

{-# LANGUAGE TypeOperators, TemplateHaskell #-}

module Visualization.Base where

import qualified Graphics.UI.GLUT as GLUT

import Configuration.General
import PDF.Repa
import qualified Data.Array.Repa as R

main :: IO ()
main = do
    (_progName, _args) <- GLUT.getArgsAndInitialize
    _window <- GLUT.createWindow $ "Visualizer running on " ++ programName
    GLUT.displayCallback GLUT.$= display
    GLUT.mainLoop

display :: GLUT.DisplayCallback
display = do
    GLUT.clear [ GLUT.ColorBuffer ]
    p <- mapPDF (\[d1, d2] -> (1 / sqrt ((d1 ** 2) + (d2 ** 2)))) $ PDF 500 [0.1,0.1] $(uniformMDArr 2 500 1) 
    GLUT.renderPrimitive GLUT.Points $ mapM_ (\p@(x,y,z) -> constructPoint p (z,0,1/z)) $ renderPDF 0.03 p
    GLUT.flush

points :: [(GLUT.GLfloat, GLUT.GLfloat, GLUT.GLfloat)]
points = [(sin(2*pi*k/120), cos(2*pi*k/120), 0) | k <- [1..120]]

constructPoint (x,y,z) (r,g,b) = do
    GLUT.color $ GLUT.Color3 r g b
    GLUT.vertex $ GLUT.Vertex3 x y z

renderPDF :: Double -> PDF ((R.Z R.:. Int) R.:. Int) -> [(GLUT.GLfloat, GLUT.GLfloat, GLUT.GLfloat)]
renderPDF sc (PDF ds dsc a) = [(mapCoord 0 x, mapCoord 1 y, realToFrac (a R.! (R.Z R.:. x R.:. y))) | x <- [0..ds - 1], y <- [0..ds - 1]]
    where mapCoord i = realToFrac . ((*) sc) . ((*) (dsc !! i)) . ((+) 0.5) . ((-) (fromIntegral ds / 2.0)) . fromIntegral
