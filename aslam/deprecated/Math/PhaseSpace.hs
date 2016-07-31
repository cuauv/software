{-
  ASLAM.Math.PhaseSpace

  Abstract Phase Spaces (probabilistic state descriptions).
-}

module PhaseSpace where

import Auxiliary

import Control.Monad

import qualified System.Random as SR
import System.IO.Unsafe

type Probability = Double

data PhaseSpace a = PhaseSpace { evaluate :: a -> Probability }

initial = PhaseSpace $ \_ -> 1.0
(>>>) x y = PhaseSpace $ \p -> evaluate x p * evaluate y p

{-
sample p rs i = 
  let deltas = zip (map fst rs) $ map (uncurry (flip (-))) rs
      points = [[fst r + (snd r * (n / i)) | n <- [0.5 .. i - 0.5]] | r <- deltas]
      evaluated = [((x, y, z), evaluate p (x, y, z)) | x <- points !! 0, y <- points !! 1, z <- points !! 2] in
  evaluated

distribution x y n = replicateM n $ SR.randomRIO (x, y)

integrateMonteCarlo f x y = let ns = 1000 in distribution x y ns >>| \d -> sum (map f d) * (y - x) / ns

-- fold3 :: Double -> Double -> PhaseSpace (Double, Double, Double) -> PhaseSpace (Double, Double)
fold3X3 x' y' p = PhaseSpace $ \( !x, !y) -> unsafePerformIO (integrateMonteCarlo (\ !z -> evaluate p (x, y, z)) x' y')
fold3X2 x' y' p = PhaseSpace $ \( !x, !y) -> unsafePerformIO (integrateMonteCarlo (\ !z -> evaluate p (x, z, y)) x' y')
fold3X1 x' y' p = PhaseSpace $ \( !x, !y) -> unsafePerformIO (integrateMonteCarlo (\ !z -> evaluate p (z, x, y)) x' y')

fold2X2 x' y' p = PhaseSpace $ \ !x -> unsafePerformIO (integrateMonteCarlo (\ !y -> evaluate p (x, y)) x' y')
fold2X1 x' y' p = PhaseSpace $ \ !x -> unsafePerformIO (integrateMonteCarlo (\ !y -> evaluate p (y, x)) x' y')

fold1X1 x' y' p = unsafePerformIO (integrateMonteCarlo (\ !x -> evaluate p x) x' y')

mean rs = fold1X1 (fst $ rs !! 0) (snd $ rs !! 0) . fold2X2 (fst $ rs !! 1) (snd $ rs !! 1) . fold3X3 (fst $ rs !! 2) (snd $ rs !! 2)
-- Integration: sample to determine cutoff bounds.
-}
