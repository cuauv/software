-- ASLAM Generic Bayesian Filter
-- Christopher Goes, 2014

-- TODO: This really should be rewritten using delayed arrays for fusion. Also, TemplateHaskell is ugly...
-- Rewrite this using time decay properly (and more typeclasses, less ugliness, etc)

{-# Language ExistentialQuantification #-}

module Core.GenericBayesian where

import Language.Haskell.TH
import Control.Monad (join)

import PDF.Repa
import qualified Data.Array.Repa as R

class StateVector a where
    relativeProb :: a -> ([Double] -> Double)

-- To use: createBayesianFilter 500 [10.0] $(uniformMDArr 1 500 1)
createBayesianFilter :: (R.Shape a) => Int -> [Double] -> R.Array R.U a Double -> BayesianFilter a
createBayesianFilter dimSize dimRanges arr = 
    let dimScale = map (/(fromIntegral dimSize)) dimRanges in
    BayesianFilter $ PDF dimSize dimScale arr

data BayesianFilter a = (R.Shape a) => BayesianFilter { pdf :: PDF a }

update :: (R.Shape a, StateVector b) => BayesianFilter a -> b -> IO (BayesianFilter a)
update init obs = fmap BayesianFilter $ mapPDF (relativeProb obs) $ pdf init

fuse :: (R.Shape a, StateVector b) => BayesianFilter a -> [b] -> IO (BayesianFilter a)
fuse init obss = foldl (\bf sv -> (\b -> update b sv) =<< bf) (return init) obss 

clear :: (R.Shape a) => BayesianFilter a -> IO (BayesianFilter a)
clear bf = fmap BayesianFilter $ mapPDF (\_ -> 1.0) $ pdf bf

data GaussianStateVector = GaussianStateVector { expected :: [Double], stddev :: [Double] }

instance StateVector GaussianStateVector where
    relativeProb (GaussianStateVector e d) dims = foldl (\net (e, d, x) -> net * (normal e d x)) 1.0 $ zip3 e d dims 

normal expected stddev val = (1.0 / (stddev * sqrt (2 * pi))) * exp (- (val - expected)**2 / (2 * stddev ** 2))

autoFusion inputs bf = centroidPDF =<< (normalizePDF . pdf) =<< (\bf -> fuse bf (map (\(a, b) -> GaussianStateVector a b) inputs)) =<< clear bf
