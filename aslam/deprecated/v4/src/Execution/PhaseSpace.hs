module Execution.PhaseSpace where

import           Control.Applicative
import qualified Data.Vector                  as V
import qualified Data.Vector.Algorithms.Intro as VA
import           System.IO.Unsafe

import           Protocol.DSL

instance Functor PhaseSpace where
  {-# INLINE fmap #-}
  fmap f = unwrapped $ V.map (\(x, y) -> (f x, y))

instance Applicative PhaseSpace where
  {-# INLINE pure #-}
  pure = PhaseSpace . V.singleton . flip (,) 1.0
  {-# INLINE (<*>) #-}
  (<*>) (PhaseSpace f) (PhaseSpace x) = PhaseSpace $ concatV $ V.map (\(f, f') -> V.map (\(x, x') -> (f x, f' * x')) x) f

instance Alternative PhaseSpace where
  {-# INLINE empty #-}
  empty = PhaseSpace V.empty
  {-# INLINE (<|>) #-}
  (<|>) (PhaseSpace x) (PhaseSpace y) = PhaseSpace $ x V.++ y

instance Monad PhaseSpace where
  {-# INLINE (>>=) #-}
  (>>=) (PhaseSpace x) f = PhaseSpace $ concatV $ V.map (\(x, x') -> unPhaseSpace $ prior x' $ f x) x
  {-# INLINE return #-}
  return = pure

{-# INLINE argmax #-}
argmax (PhaseSpace x) = if x /= V.empty then Just $ V.maximumBy (\(_, x) (_, y) -> x `compare` y) x else Nothing

{-# INLINE apriori #-}
apriori = PhaseSpace . V.map (flip (,) 1)

{-# INLINE predicate #-}
predicate f = chain $ \x -> if f x then 1 else 0

{-# INLINE chain #-}
chain f = unwrapped $ V.map (\(x, y) -> (x, y * f x))

{-# INLINE prior #-}
prior = chain . const

{-# INLINE unwrapped #-}
unwrapped f = PhaseSpace . f . unPhaseSpace

{-# INLINE concatV #-}
concatV = V.foldl' (V.++) V.empty

{-# INLINE limit #-}
limit n = PhaseSpace . V.take n . sortV (\(_, px) (_, py) -> py `compare` px) . unPhaseSpace

{-# INLINE sortV #-}
sortV f v =
  unsafePerformIO $ do
    t <- V.thaw v
    VA.sortBy f t
    V.freeze t

{-# INLINE normalize #-}
normalize (PhaseSpace x) = let t = V.sum $ V.map snd x in PhaseSpace $ V.map (\(x, p) -> (x, p / t)) x

{-# INLINE fuse #-}
fuse (PhaseSpace x) (PhaseSpace y) = PhaseSpace $ concatV $ V.map (\(x, xp) -> V.map (\(y, yp) -> (V.cons y x, xp * yp)) y) x

{-# INLINE fuseAll #-}
fuseAll xs = V.foldr' (flip fuse) (return V.empty) $ xs
