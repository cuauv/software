module Math.PhaseSpace where

import           Data.Default
import qualified Data.Vector.Algorithms.Intro as VA
import qualified Data.Vector.Unboxed          as UV
import           System.IO.Unsafe

type PhaseSpace a = UV.Vector (a, Double)

mapAcross ∷ forall a b . (UV.Unbox a, UV.Unbox b) ⇒ (a → b) → PhaseSpace a → PhaseSpace b
mapAcross f = UV.map (\(x, y) -> (f x, y))
{-# INLINE mapAcross #-}

apriori ∷ forall a . UV.Unbox a ⇒ UV.Vector a → PhaseSpace a
apriori = UV.map (flip (,) 1.0)
{-# INLINE apriori #-}

argmax ∷ forall a . (Default a, UV.Unbox a) ⇒ PhaseSpace a → (a, Double)
argmax x = if UV.length x > 0 then UV.maximumBy (\(_, x) (_, y) -> x `compare` y) x else (def, 0)
{-# INLINE argmax #-}

chain ∷ forall a . UV.Unbox a ⇒ (a → Double) → PhaseSpace a → PhaseSpace a
chain f = UV.map (\(x, y) -> (x, y * f x))
{-# INLINE chain #-}

predicate ∷ forall a . UV.Unbox a ⇒ (a → Bool) → PhaseSpace a → PhaseSpace a
predicate f = chain $ \x -> if f x then 1 else 0
{-# INLINE predicate #-}

prior ∷ forall a . UV.Unbox a ⇒ Double → PhaseSpace a → PhaseSpace a
prior = chain . const
{-# INLINE prior #-}

normalize ∷ forall a . UV.Unbox a ⇒ PhaseSpace a → PhaseSpace a
normalize x = let !total = UV.sum $ UV.map snd x in UV.map (\(x, y) -> (x, y / total)) x
{-# INLINE normalize #-}

sortV ∷ forall a . UV.Unbox a ⇒ (a → a → Ordering) → UV.Vector a → UV.Vector a
sortV f v =
  unsafePerformIO $ do
    t <- UV.thaw v
    VA.sortBy f t
    UV.unsafeFreeze t
{-# INLINE sortV #-}

limit ∷ forall a . UV.Unbox a ⇒ Int → PhaseSpace a → PhaseSpace a
limit n = UV.take n . sortV (\(_, px) (_, py) -> py `compare` px)
{-# INLINE limit #-}

fuse ∷ forall a b . (UV.Unbox a, UV.Unbox b) ⇒ PhaseSpace a → PhaseSpace b → PhaseSpace (a, b)
fuse x y = UV.concatMap (\(y', yp') -> UV.map (\(x', xp') -> ((x', y'), xp' * yp')) x) y
{-# INLINE fuse #-}

fuse3 ∷ forall a b c . (UV.Unbox a, UV.Unbox b, UV.Unbox c) ⇒ PhaseSpace a → PhaseSpace b → PhaseSpace c → PhaseSpace (a, b, c)
fuse3 x y z = UV.concatMap (\(z', zp') -> UV.concatMap (\(y', yp') -> UV.map (\(x', xp') -> ((x', y', z'), xp' * yp' * zp')) x) y) z
{-# INLINE fuse3 #-}
 
fuse4 :: forall a b c d . (UV.Unbox a, UV.Unbox b, UV.Unbox c, UV.Unbox d) => PhaseSpace a -> PhaseSpace b -> PhaseSpace c -> PhaseSpace d -> PhaseSpace (a, b, c, d)
fuse4 x y z w = UV.concatMap (\(z', zp') -> UV.concatMap (\(y', yp') -> UV.concatMap (\(x', xp') -> UV.map (\(w', wp') -> ((x', y', z', w'), xp' * yp' * zp' * wp')) w) x) y) z
{-# INLINE fuse4 #-}
