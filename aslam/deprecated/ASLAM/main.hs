{-# LANGUAGE TemplateHaskell #-}

import Core.GenericBayesian
import PDF.Repa

import Control.Monad (forever)

autoFusion3 inp ranges = autoFusion inp $ createBayesianFilter 50 ranges $(uniformMDArr 3 50 1)

main = --forever $ do
    putStrLn . show =<< autoFusion3 [([1.0,1.0,1.0],[0.5,0.5,0.5]), ([2.0,2.0,2.0],[1.0,1.0,1.0])] [50,50,50]
{-
main = forever $ do
    line <- getLine
    out <- (\(x,y) -> autoFusion3 x y) $ read line
    putStrLn $ show out
-}
