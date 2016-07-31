{- 
  ASLAM Math 
-}

module Math where

import Auxiliary

import Control.Monad -- TEMPORARY

import qualified Data.Array.Repa as R
import qualified Statistics.Sample as S
-- import qualified Data.Array.Repa.Unsafe as R

{- MDArray: Multidimensional Array -}

type MDArray a = R.Array R.U a Double

{-# INLINE (~~>) #-}
(~~>) :: (R.Shape a) => MDArray a -> (a -> Double) -> IO (MDArray a)
(~~>) !p !f = R.computeUnboxedP $ R.traverse p id (\ !l !x -> l x * f x)

{-# INLINE (~|>) #-}
(~|>) :: (R.Shape a) => MDArray a -> ((a -> Double) -> a -> Double) -> IO (MDArray a)
(~|>) !p !f = R.computeUnboxedP $ R.traverse p id f

{-# INLINE (~~^) #-}
(~~^) :: (R.Shape a) => MDArray a -> Double -> IO (MDArray a)
(~~^) !p !d = R.computeUnboxedP $ R.map (\ !x -> x ** d) p

{-# INLINE (~||) #-}
(~||) !p = R.sumAllP p >>= \s -> R.computeUnboxedP (R.map (\ !x -> x / s) p)

{- PDF: MDArray + Ranges -}

data PDF = PDF ![(Double, Double)] !(MDArray R.DIM3) deriving (Show)

scale e = fromIntegral (R.size (R.extent e)) ** (1/3)

(PDF r e) `updatedWith` y = 
  let !scale' = scale e
      xm i = (fst $ r !! 0) + (snd (r !! 0) - fst (r !! 0)) * (fromIntegral i + 0.5) / scale'
      ym j = (fst $ r !! 1) + (snd (r !! 1) - fst (r !! 1)) * (fromIntegral j + 0.5) / scale'
      zm k = (fst $ r !! 2) + (snd (r !! 2) - fst (r !! 2)) * (fromIntegral k + 0.5) / scale'
  in PDF r |<< (e ~~> (\ !(R.Z R.:. i R.:. j R.:. k) -> y (xm i) (ym j) (zm k)))

(PDF r e) `raisedTo` p = PDF r |<< (e ~~^ p)

(PDF r e) `movedBy` (x, y, z) = 
  let !scale' = scale e
      (!xd, !yd, !zd) = (- x / scale', - y / scale', - z / scale')
      (!(!x0, !x0w), !(!x1, !x1w)) = ((floor xd, fromIntegral (ceiling xd) - xd), (ceiling xd, xd - fromIntegral (floor xd)))
      (!(!y0, !y0w), !(!y1, !y1w)) = ((floor yd, fromIntegral (ceiling yd) - yd), (ceiling yd, yd - fromIntegral (floor yd)))
      (!(!z0, !z0w), !(!z1, !z1w)) = ((floor zd, fromIntegral (ceiling zd) - zd), (ceiling zd, zd - fromIntegral (floor zd)))
      ensure !i = max 0 (min i $ round scale' - 1)
  in PDF r |<< (e ~|> (\ !l !(R.Z R.:. i R.:. j R.:. k) -> 
    let !xn0 = ensure (i + x0)
        !xn1 = ensure (i + x1)
        !yn0 = ensure (j + y0)
        !yn1 = ensure (j + y1)
        !zn0 = ensure (k + z0)
        !zn1 = ensure (k + z1) in
    l (R.Z R.:. xn0 R.:. yn0 R.:. zn0) * x0w * y0w * z0w +
    l (R.Z R.:. xn0 R.:. yn0 R.:. zn1) * x0w * y0w * z1w +
    l (R.Z R.:. xn0 R.:. yn1 R.:. zn0) * x0w * y1w * z0w +
    l (R.Z R.:. xn0 R.:. yn1 R.:. zn1) * x0w * y1w * z1w +
    l (R.Z R.:. xn1 R.:. yn0 R.:. zn0) * x1w * y0w * z0w +
    l (R.Z R.:. xn1 R.:. yn0 R.:. zn1) * x1w * y0w * z1w +
    l (R.Z R.:. xn1 R.:. yn1 R.:. zn0) * x1w * y1w * z0w +
    l (R.Z R.:. xn1 R.:. yn1 R.:. zn1) * x1w * y1w * z1w ))

centroid (PDF r e) =
  let !scale' = round (scale e) :: Int
      xm i = (fst $ r !! 0) + (snd (r !! 0) - fst (r !! 0)) * (i + 0.5) / fromIntegral scale'
      ym j = (fst $ r !! 1) + (snd (r !! 1) - fst (r !! 1)) * (j + 0.5) / fromIntegral scale'
      zm k = (fst $ r !! 2) + (snd (r !! 2) - fst (r !! 2)) * (k + 0.5) / fromIntegral scale' in
  (\(i, j, k) -> (xm i, ym j, zm k)) $ 
    foldl (\ ( !i, !j, !k) ( !i', !j', !k') -> let v = e R.! (R.Z R.:. i' R.:. j' R.:. k') in (i + (fromIntegral i' * v), j + (fromIntegral j' * v), k + (fromIntegral k' * v)))
      (0, 0, 0) [(i, j, k) | i <- [0 .. scale' - 1], j <- [0 .. scale' - 1], k <- [0 .. scale' - 1]]

normalize (PDF r e) = PDF r |<< ((~||) e)

variance (PDF r e) = 
  let !scale' = scale e
      !scalei = round scale'
      xm i = (snd (r !! 0) - fst (r !! 0)) * i / scale'
      ym j = (snd (r !! 1) - fst (r !! 1)) * j / scale'
      zm k = (snd (r !! 2) - fst (r !! 2)) * k / scale' 
      var = S.stdDev . R.toUnboxed in do
  idistr <- R.computeP $ R.traverse e (\_ -> (R.Z R.:. scalei)) (\ !l !(R.Z R.:. i) -> xm $ fromIntegral i * foldl (\ !acc !x -> acc + x) 0 [l (R.Z R.:. i R.:. j R.:. k) | j <- [0 .. scalei - 1], k <- [0 .. scalei - 1]])
  jdistr <- R.computeP $ R.traverse e (\_ -> (R.Z R.:. scalei)) (\ !l !(R.Z R.:. j) -> ym $ fromIntegral j * foldl (\ !acc !x -> acc + x) 0 [l (R.Z R.:. i R.:. j R.:. k) | i <- [0 .. scalei - 1], k <- [0 .. scalei - 1]])
  kdistr <- R.computeP $ R.traverse e (\_ -> (R.Z R.:. scalei)) (\ !l !(R.Z R.:. k) -> zm $ fromIntegral k * foldl (\ !acc !x -> acc + x) 0 [l (R.Z R.:. i R.:. j R.:. k) | i <- [0 .. scalei - 1], j <- [0 .. scalei - 1]])
  return (var idistr, var jdistr, var kdistr)

sample (PDF r e) = 
  let !scale' = round $ scale e
      xm i = (fst $ r !! 0) + (snd (r !! 0) - fst (r !! 0)) * (fromIntegral i + 0.5) / fromIntegral scale'
      ym j = (fst $ r !! 1) + (snd (r !! 1) - fst (r !! 1)) * (fromIntegral j + 0.5) / fromIntegral scale'
      zm k = (fst $ r !! 2) + (snd (r !! 2) - fst (r !! 2)) * (fromIntegral k + 0.5) / fromIntegral scale'
  in [(xm i, ym j, zm k, e R.! (R.Z R.:. i R.:. j R.:. k)) | i <- [0, 10 .. scale' - 1], j <- [0, 10 .. scale' - 1], k <- [0, 10 .. scale' - 1]]

-- Faster version; no interpolation. Should average out (?).
(PDF r e) `movedBy2` (x, y, z) =
  let !scale' = scale e
      (!xd, !yd, !zd) = (round $ - x * scale' / (snd (r !! 0) - fst (r !! 0)), round $ - y * scale' / (snd (r !! 1) - fst (r !! 1)), round $ - z * scale' / (snd (r !! 0) - fst (r !! 0)))
      ensure !i = max 0 (min i $ round scale' - 1)
  in PDF r |<< (e ~|> (\ !l !(R.Z R.:. i R.:. j R.:. k) -> l (R.Z R.:. ensure (i + xd) R.:. ensure (j + yd) R.:. ensure (k + zd)) ))

{- Utility Functions -}

{-# INLINE gaussian #-}
gaussian :: Double -> Double -> Double -> Double
gaussian !x !mu !variance = exp $ - ((x - mu) ** 2) / (2 * variance)

{-# INLINE directionSimilarity #-} 
-- Desired -> Source -> Dest -> Similarity
directionSimilarity :: (Double, Double, Double) -> (Double, Double, Double) -> Double -> Double -> Double -> Double
directionSimilarity (!dx, !dy, !dz) (!ox, !oy, !oz) x y z = 
  let !dx' = x - ox
      !dy' = y - oy
      !dz' = z - oz in
  (dx * dx' + dy * dy' + dz * dz') / ((dx' ** 2.0) + (dy' ** 2.0) + (dz' ** 2.0)) ** 0.5

new :: [(Double, Double)] -> PDF
new r = PDF r $ R.fromListUnboxed (R.Z R.:. 200 R.:. 200 R.:. 200) $ take (200 ^ 3) $ repeat 1.0

-- TEMPORARY

x :: MDArray R.DIM3
x = R.fromListUnboxed (R.Z R.:. 200 R.:. 200 R.:. 200) $ take (200 ^ 3) $ repeat 1.0

y = PDF [(-1, 1), (-1, 1), (-1, 1)] x -- (\_ -> 1.0)

test = x `seq` replicateM 100 $ print =<< (centroid |<< normalize y)
