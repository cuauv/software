-- ASLAM Generic Exports

{-# LANGUAGE TemplateHaskell #-}

module Export.Generic where

import PDF.Repa
import Core.GenericBayesian

autoFusion3 :: [([Double],[Double])] -> [Double] -> IO [Double]
autoFusion3 inp ranges = autoFusion inp $ createBayesianFilter 500 ranges $(uniformMDArr 3 500 1)
