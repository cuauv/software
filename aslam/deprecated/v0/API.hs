{- 
  ASLAM Core: Application Programming Interface
  
  (c) Christopher Goes 2015
-}

module API where

import Auxiliary
import Networking
import qualified Math

import Control.Concurrent
import Data.Maybe

import qualified Data.Aeson.TH as A
import qualified Data.Map as M

-- Generics.

data Gaussian a = Gaussian { gEstimated :: a, gUncertainty :: a } deriving (Show)

-- Phase spaces / objects.

data Ranges = Ranges { rXMIN :: Double, rXMAX :: Double, rYMIN :: Double, rYMAX :: Double, rZMIN :: Double, rZMAX :: Double } deriving (Show, Eq)
data PhaseSpace = PhaseSpace { psName :: String, psRanges :: Ranges } deriving (Show, Eq)
data Object = Object { objName :: String, objPhaseSpace :: PhaseSpace } deriving (Show, Eq)

-- Observations (data in).

data Vector3 = Vector3 { vX :: Double, vY :: Double, vZ :: Double } deriving (Show)

-- Directional ~ Unit vector. Spanning ~ distance.

data BipartiteData = Directional (Gaussian Vector3) | Spanning (Gaussian Double) deriving (Show)

-- Locational: position. Relative: movement.
data MonopartiteData = Locational (Gaussian Vector3) | Relative (Gaussian Vector3) deriving (Show)

data Observation = Bipartite { boFrom :: String, boTo :: String, boData :: BipartiteData } | Monopartite { moObject :: String, moData :: MonopartiteData } deriving (Show)

-- Extrapolations (data out).

data Extrapolation = Position { pObject :: String } deriving (Show)
data ExtrapolationR = PositionR (Gaussian Vector3) deriving (Show)

data Request = Register Object | Update Observation | Acquire Extrapolation | Sample String deriving (Show)
data Response = Registered | Updated | Acquired ExtrapolationR | Sampled [(Double, Double, Double, Double)] | Error String deriving (Show)

$(A.deriveJSON defaultJSONOptions ''Gaussian)
$(A.deriveJSON defaultJSONOptions ''Ranges)
$(A.deriveJSON defaultJSONOptions ''PhaseSpace)
$(A.deriveJSON defaultJSONOptions ''Object)
$(A.deriveJSON defaultJSONOptions ''Vector3)
$(A.deriveJSON defaultJSONOptions ''BipartiteData)
$(A.deriveJSON defaultJSONOptions ''MonopartiteData)
$(A.deriveJSON defaultJSONOptions ''Observation)
$(A.deriveJSON defaultJSONOptions ''Extrapolation)
$(A.deriveJSON defaultJSONOptions ''ExtrapolationR)
$(A.deriveJSON defaultJSONOptions ''Request)
$(A.deriveJSON defaultJSONOptions ''Response)

data State = State {
  objects :: M.Map String Object,
  raw :: M.Map String (MVar Math.PDF)
}

handle :: MVar State -> Request -> IO Response

handle state (Register o@(Object n ps)) = do
  e <- (isJust . M.lookup n . objects) |<< readMVar state
  if e then return (Error "Object already exists.") else do
    modifyMVar_ state $ \s -> do
      pm <- newMVar $ Math.new [(rXMIN $ psRanges ps, rXMAX $ psRanges ps), (rYMIN $ psRanges ps, rYMAX $ psRanges ps), (rZMIN $ psRanges ps, rZMAX $ psRanges ps)]
      return s {
        objects = M.insert n o (objects s),
        raw = M.insert n pm (raw s)
      }
    return Registered

handle state (Acquire (Position n)) = do
  pdf <- (M.lookup n . raw) |<< readMVar state
  case pdf of
    Just pdf -> do
      pdf <- readMVar pdf
      (x, y, z) <- Math.centroid |<< Math.normalize pdf
      (vx, vy, vz) <- Math.variance pdf
      return $ Acquired $ PositionR $ Gaussian (Vector3 x y z) (Vector3 vx vy vz)
    Nothing -> return $ Error "Object not found."

handle state (Sample n) = do
  pdf <- (M.lookup n . raw) |<< readMVar state
  case pdf of
    Just pdf -> do
      pdf <- readMVar pdf
      return $ Sampled $ Math.sample pdf
    Nothing -> return $ Error "Object not found."

handle state (Update o) = do
  case o of
    Monopartite n d -> do
      pdf <- (M.lookup n . raw) |<< readMVar state
      case pdf of
        Just pdf -> do
          case d of
            Locational (Gaussian (Vector3 ex ey ez) (Vector3 vx vy vz)) -> 
              modifyMVar_ pdf $ \p -> Math.normalize =<< p `Math.updatedWith` (\ !x !y !z -> Math.gaussian x ex vx * Math.gaussian y ey vy * Math.gaussian z ez vz)
            Relative (Gaussian (Vector3 dx dy dz) _) ->
              modifyMVar_ pdf $ \p -> p `Math.movedBy2` (dx, dy, dz)
          return $ Updated
        Nothing -> return $ Error "Object not found."
    Bipartite f t d -> do
      from <- (M.lookup f . raw) |<< readMVar state
      to <- (M.lookup t . raw) |<< readMVar state
      case (from, to) of
        (Just from, Just to) -> do
          case d of
            Directional (Gaussian (Vector3 ex ey ez) (Vector3 vx vy vz)) -> do
              (fpx, fpy, fpz) <- Math.centroid |<< readMVar from
              (tpx, tpy, tpz) <- Math.centroid |<< readMVar to
              modifyMVar_ to $ \p -> Math.normalize =<< p `Math.updatedWith`
                \ !x !y !z -> let (!dx, !dy, !dz) = (x - fpx, y - fpy, z - fpz)
                                  !norm = ((dx ** 2.0) + (dy ** 2.0) + (dz ** 2.0)) ** 0.5
                                  (!dx', !dy', !dz') = (dx / norm, dy / norm, dz / norm) in
                              max (((dx' * ex) + (dy' * ey) + (dz' * ez)) ** 3.0) 0
              {- modifyMVar_ from $ \p -> Math.normalize =<< p `Math.updatedWith`
                \ !x !y !z -> let (!dx, !dy, !dz) = (tpx - x, tpy - y, tpz - z)
                                  !norm = ((dx ** 2.0) + (dy ** 2.0) + (dz ** 2.0)) * 0.5
                                  (!dx', !dy', !dz') = (dx / norm, dy / norm, dz / norm) in
                              Math.gaussian dx' ex vx *
                              Math.gaussian dy' ey vy * 
                              Math.gaussian dz' ez vz -}
            Spanning (Gaussian ed vd) -> do
              (!fex, !fey, !fez) <- Math.centroid |<< (Math.normalize =<< readMVar from)
              (!tex, !tey, !tez) <- Math.centroid |<< (Math.normalize =<< readMVar to)
              modifyMVar_ to $ \p -> Math.normalize =<< p `Math.updatedWith`
                \ !x !y !z -> let (!dx, !dy, !dz) = (x - fex, y - fey, z - fez)
                                  !norm = ((dx ** 2.0) + (dy ** 2.0) + (dz ** 2.0)) ** 0.5 in
                              Math.gaussian norm ed vd
              {- modifyMVar_ from $ \p -> Math.normalize =<< p `Math.updatedWith`
                \ !x !y !z -> let (!dx, !dy, !dz) = (x - tex, y - tey, z - tez)
                                  !norm = ((dx ** 2.0) + (dy ** 2.0) + (dz ** 2.0)) ** 0.5 in
                              Math.gaussian norm ed vd -}
          return Updated
        _ -> return $ Error "Object not found." 

run = do
  state <- newMVar $ State M.empty M.empty
  serveSynchronously (toZMQ "127.0.0.1" 6666) (wrapped (handle state) (return $ Error "Invalid JSON."))
