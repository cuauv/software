module Math.Aux where

import           Data.Fixed

gaussian ∷ Double → Double → Double → Double
gaussian !x !mu !variance = exp $ - ((x - mu) ** 2) / (2 * variance)
{-# INLINE gaussian #-}

gaussianH ∷ Double → Double → Double → Double
gaussianH !x !mu !variance = exp $ - ((hdiff x mu) ** 2) / (2 * variance)
{-# INLINE gaussianH #-}

hdiff :: Double -> Double -> Double
hdiff x y = let d = (x - y) `mod'` (2 * pi) in if d > pi then (2 * pi) - d else d
{-# INLINE hdiff #-}

heading2 ∷ (Double, Double) → (Double, Double) → Double
heading2 (!x1, !y1) (!x2, !y2) = atan2 (y2 - y1) (x2 - x1)
{-# INLINE heading2 #-}

distance2 ∷ (Double, Double) → (Double, Double) → Double
distance2 (!x1, !y1) (!x2, !y2) = ( ((x2 - x1) ** 2.0) + ((y2 - y1) ** 2.0) ) ** 0.5
{-# INLINE distance2 #-}
