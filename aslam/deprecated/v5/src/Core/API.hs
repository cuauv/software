module Core.API where

import           Data.Default
import           Data.Random.Normal
import qualified Data.Vector.Unboxed as UV

import           Math

{-
  Cumulative Resampling
  Transform hypothesis probabilities into hypothesis frequencies.
  Preserves probability distributions.

  Precondition: Normalized
  (function could be optimized a bit)
-}
resampleCumulative ∷ forall a . (UV.Unbox a, Numerical a) ⇒ Int → PhaseSpace a → PhaseSpace a
resampleCumulative n s =
  let cumulative  = UV.postscanl' (\(_, _, c) (x, p) -> (x, p, c + p)) (zero, 0, 0) s
      frac        = 1 / fromIntegral n
      indices     = UV.enumFromStepN 0 frac n
      go vec inds =
        if UV.length inds == 0 then UV.empty else
          let (prob, remi) = UV.splitAt 1 inds
              probV        = prob UV.! 0
              valid        = UV.dropWhile ((>) probV . thd3) vec
              (cX, _, _)   = UV.head valid
          in (cX, frac) `UV.cons` go valid remi
      {-# INLINE go #-}
  in go cumulative indices
{-# INLINE resampleCumulative #-}

splitSample ∷ forall a . (Numerical a, UV.Unbox a) ⇒ Double → PhaseSpace a → PhaseSpace a → PhaseSpace a
splitSample frac x y =
  let len = UV.length x
      nX  = round $ frac * fromIntegral len
      nY  = len - nX
  in resampleCumulative nX x UV.++ resampleCumulative nY y
{-# INLINE splitSample #-}

autoresample :: forall a . (Numerical a, UV.Unbox a) => PhaseSpace a -> PhaseSpace a
autoresample x = resampleCumulative (UV.length x) x

deviance :: forall a . (Numerical a, UV.Unbox a) => PhaseSpace a -> Double
deviance var = let mean = weightedMean var in weightedMean $ mapAcross ((.\\) mean) var
{-# INLINE deviance #-}

weightedMean ∷ forall a . (Numerical a, UV.Unbox a) ⇒ PhaseSpace a → a
weightedMean = UV.foldl' (.+) zero . UV.map (\(x, y) -> x .* y)
{-# INLINE weightedMean #-}

noiseGen ∷ forall a . (Numerical a, UV.Unbox a) ⇒ Int → a → IO (UV.Vector a)
noiseGen len uncer = UV.replicateM len (err uncer)
{-# INLINE noiseGen #-}

fst3 ∷ (a, b, c) → a
fst3 (x, _, _) = x
{-# INLINE fst3 #-}

snd3 ∷ (a, b, c) → b
snd3 (_, y, _) = y
{-# INLINE snd3 #-}

thd3 ∷ (a, b, c) → c
thd3 (_, _, z) = z
{-# INLINE thd3 #-}

fst4 ∷ (a, b, c, d) → a
fst4 (a, _, _, _) = a
{-# INLINE fst4 #-}

snd4 ∷ (a, b, c, d) → b
snd4 (_, b, _, _) = b
{-# INLINE snd4 #-}

thd4 ∷ (a, b, c, d) → c
thd4 (_, _, c, _) = c
{-# INLINE thd4 #-}

fth4 ∷ (a, b, c, d) → d
fth4 (_, _, _, d) = d
{-# INLINE fth4 #-}

{- Typeclass Overloads (~= C++ templating) -}

class Applicable a b | a -> b where
  apply ∷ (b → Double) → a → a

instance forall a . UV.Unbox a ⇒ Applicable (PhaseSpace a) a where
  apply = chain
  {-# INLINE apply #-}

instance forall a b . (Default a, Default b, UV.Unbox a, UV.Unbox b) ⇒ Applicable (PhaseSpace a, PhaseSpace b) (a, b) where
  apply f (!x, !y) =
    let !mX = fst $ argmax x
        !mY = fst $ argmax y
        !uX = apply (\x -> f (x, mY)) x
        !uY = apply (\y -> f (mX, y)) y
    in (uX, uY)
  {-# INLINE apply #-}

instance forall a b c . (Default a, Default b, Default c, UV.Unbox a, UV.Unbox b, UV.Unbox c) ⇒ Applicable (PhaseSpace a, PhaseSpace b, PhaseSpace c) (a, b, c) where
  apply f (!x, !y, !z) =
    let !mX = fst $ argmax x
        !mY = fst $ argmax y
        !mZ = fst $ argmax z
        !uX = apply (\x -> f (x, mY, mZ)) x
        !uY = apply (\y -> f (mX, y, mZ)) y
        !uZ = apply (\z -> f (mX, mY, z)) z
    in (uX, uY, uZ)
  {-# INLINE apply #-}

class Estimable a b | a -> b where
  estimateArgmax ∷ (b → c) → a → (c, Double)
  estimateWMean  ∷ (b → c) → a → c

instance forall a . (Numerical a, Default a, UV.Unbox a) ⇒ Estimable (PhaseSpace a) a where
  estimateArgmax f x = let (!v, !p) = argmax x in (f v, p)
  {-# INLINE estimateArgmax #-}

  estimateWMean f x = f $ weightedMean x
  {-# INLINE estimateWMean #-}

class Griddable a b | b -> a where
  grid ∷ Int → a → PhaseSpace b

instance Griddable (Double, Double) Double where
  grid n (s, e) =
    let delta = e - s
        num   = fromIntegral n
        prior = 1 / num
    in UV.map (\v -> (delta * fromIntegral v / num, prior)) $ UV.enumFromN (0 :: Int) n
  {-# INLINE grid #-}

instance forall a . (UV.Unbox a, Griddable (a, a) a) ⇒ Griddable ((a, a), (a, a)) (a, a) where
  grid n (x, y) =
    let gX = grid n x
        gY = grid n y
    in fuse gX gY
  {-# INLINE grid #-}

instance forall a . (UV.Unbox a, Griddable (a, a) a) ⇒ Griddable ((a, a), (a, a), (a, a)) (a, a, a) where
  grid n (x, y, z) =
    let gX = grid n x
        gY = grid n y
        gZ = grid n z
    in fuse3 gX gY gZ
  {-# INLINE grid #-}

instance forall a . (UV.Unbox a, Griddable (a, a) a) ⇒ Griddable ((a, a), (a, a), (a, a), (a, a)) (a, a, a, a) where
  grid n (x, y, z, w) =
    let gX = grid n x
        gY = grid n y
        gZ = grid n z
        gW = grid n w
    in fuse4 gX gY gZ gW
  {-# INLINE grid #-}

class (Default a, UV.Unbox a) ⇒ Numerical a where
  zero ∷ a -- Identity under addition.
  (.+) ∷ a → a → a
  (.-) ∷ a → a → a
  (.*) ∷ a → Double → a
  (./) ∷ a → Double → a
  (.**) ∷ a → Double → a
  (.<) ∷ a → a → Bool
  (.>) ∷ a → a → Bool
  (.==) ∷ a → a → Bool
  (.\\) ∷ a → a → Double -- Planar distance.
  err ∷ a → IO a -- Generate uncertainty.

instance Numerical Double where
  zero = 0.0
  {-# INLINE zero #-}

  (.+) = (+)
  {-# INLINE (.+) #-}

  (.-) = (-)
  {-# INLINE (.-) #-}

  (.*) = (*)
  {-# INLINE (.*) #-}

  (./) = (/)
  {-# INLINE (./) #-}

  (.**) = (**)
  {-# INLINE (.**) #-}

  (.<) = (<)
  {-# INLINE (.<) #-}

  (.>) = (>)
  {-# INLINE (.>) #-}

  (.==) = (==)
  {-# INLINE (.==) #-}

  (.\\) x y = abs $ x - y
  {-# INLINE (.\\) #-}

  err x = normalIO' (0, x)
  {-# INLINE err #-}

instance Numerical (Double, Double) where
  zero = (zero, zero)
  {-# INLINE zero #-}

  (.+) (a, b) (c, d) = (a + c, b + d)
  {-# INLINE (.+) #-}

  (.-) (a, b) (c, d) = (a - c, b - d)
  {-# INLINE (.-) #-}

  (.*) (a, b) c = (a * c, b * c)
  {-# INLINE (.*) #-}

  (./) (a, b) c = (a / c, b / c)
  {-# INLINE (./) #-}

  (.**) (a, b) c = (a ** c, b ** c)
  {-# INLINE (.**) #-}

  (.<) (a, b) (c, d) = a < c && b < d
  {-# INLINE (.<) #-}

  (.>) (a, b) (c, d) = a > c && b > d
  {-# INLINE (.>) #-}

  (.==) = (==)
  {-# INLINE (.==) #-}

  (.\\) (a, b) (c, d) = ( ( a - c ) ** 2.0 + ( b - d ) ** 2.0 ) ** 0.5
  {-# INLINE (.\\) #-}

  err (x, y) = do
    a <- err x
    b <- err y
    return (a, b)
  {-# INLINE err #-}

instance Numerical (Double, Double, Double) where
  zero = (zero, zero, zero)
  {-# INLINE zero #-}

  (.+) (a, b, c) (d, e, f) = (a + d, b + e, c + f)
  {-# INLINE (.+) #-}

  (.-) (a, b, c) (d, e, f) = (a - d, b - e, c - f)
  {-# INLINE (.-) #-}

  (.*) (a, b, c) d = (a * d, b * d, c * d)
  {-# INLINE (.*) #-}

  (./) (a, b, c) d = (a / d, b / d, c / d)
  {-# INLINE (./) #-}

  (.**) (a, b, c) d = (a ** d, b ** d, c ** d)
  {-# INLINE (.**) #-}

  (.<) (a, b, c) (d, e, f) = a < d && b < e && c < f
  {-# INLINE (.<) #-}

  (.>) (a, b, c) (d, e, f) = a > d && b > e && c > f
  {-# INLINE (.>) #-}

  (.==) = (==)
  {-# INLINE (.==) #-}

  (.\\) (a, b, c) (d, e, f) = ( ( a - d ) ** 2.0 + ( b - e ) ** 2.0 + ( c - f ) ** 2.0 ) ** 0.5
  {-# INLINE (.\\) #-}

  err (x, y, z) = do
    a <- err x
    b <- err y
    c <- err z
    return (a, b, c)
  {-# INLINE err #-}

instance Numerical (Double, Double, Double, Double) where
  zero = (zero, zero, zero, zero)
  {-# INLINE zero #-}

  (.+) (a, b, c, d) (e, f, g, h) = (a + e, b + f, c + g, d + h)
  {-# INLINE (.+) #-}

  (.-) (a, b, c, d) (e, f, g, h) = (a - e, b - f, c - g, d - h)
  {-# INLINE (.-) #-}

  (.*) (a, b, c, d) e = (a * e, b * e, c * e, d * e)
  {-# INLINE (.*) #-}

  (./) (a, b, c, d) e = (a / e, b / e, c / e, d / e)
  {-# INLINE (./) #-}

  (.**) (a, b, c, d) e = (a ** e, b ** e, c ** e, d ** e)
  {-# INLINE (.**) #-}

  (.<) (a, b, c, d) (e, f, g, h) = a < e && b < f && c < g && d < h
  {-# INLINE (.<) #-}

  (.>) (a, b, c, d) (e, f, g, h) = a > e && b > f && c > g && d > h
  {-# INLINE (.>) #-}

  (.==) = (==)
  {-# INLINE (.==) #-}

  (.\\) (a, b, c, d) (e, f, g, h) = ( ( a - e ) ** 2.0 + ( b - f ) ** 2.0 + ( c - g ) ** 2.0 + ( d - h ) ** 2.0 ) ** 0.5
  {-# INLINE (.\\) #-}

  err (x, y, z, h) = do
    a <- err x
    b <- err y
    c <- err z
    d <- err h
    return (a, b, c, d)
  {-# INLINE err #-}
