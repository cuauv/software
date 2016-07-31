{- ASLAM Generic Functionality in pure Haskell, mainly for demonstration purposes. -}

module HaskellImpl where

import Auxiliary
import Types

import Control.Concurrent
import Data.Fixed (mod')
import qualified Data.HashMap.Strict as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

granularity = 200

-- Env -> Exp -> Either String Repr
evaluate :: M.HashMap String Repr -> Exp -> Either String Repr
evaluate env (AddE x y) = numeric env (+) x y
evaluate env (SubE x y) = numeric env (-) x y
evaluate env (MulE x y) = numeric env (*) x y
evaluate env (DivE x y) = numeric env (/) x y
evaluate env (ModE x y) = scalar env (\x -> fromRational $ toRational $ toRational x `mod'` y) x
evaluate env (SinE x) = scalar env sin x
evaluate env (CosE x) = scalar env cos x
evaluate env (TanE x) = scalar env tan x
evaluate env (GaussianE x y z) = do
  x <- evaluate env x
  y <- evaluate env y
  z <- evaluate env z
  case (x, y, z) of
    (RealR x, RealR y, RealR z) -> return $ RealR $ gaussian x y z
    _ -> fail "Real scalar required."
evaluate env (DotE x y) = evaluate env (MulE x y) -- identical for rank-1 tensors
evaluate env (NormE x) = do
  x <- evaluate env x
  case x of
    VectorR x | all isReal x -> return $ RealR $ sqrt $ sum $ map (\(RealR x) -> x ** 2) x
    _ -> fail "Real vector required."
evaluate env (IndE x i) = do
  x <- evaluate env x
  case x of
    VectorR x | i >= 0 && length x > i -> return $ x !! i
    VectorR _ -> fail "Invalid index."
    _ -> fail "Vector required."
evaluate env (VecE x) = do
  r <- sequence $ map (evaluate env) x
  return $ VectorR r
evaluate _ (LitE d) = return $ RealR d
evaluate env (VarE x) = 
  case M.lookup x env of
    Just r -> return r
    Nothing -> fail "Variable not found."

extract (AddE x y) = extract x ++ extract y
extract (SubE x y) = extract x ++ extract y
extract (MulE x y) = extract x ++ extract y
extract (DivE x y) = extract x ++ extract y
extract (ModE x _) = extract x
extract (SinE x) = extract x
extract (CosE x) = extract x
extract (TanE x) = extract x
extract (GaussianE x y z) = extract x ++ extract y ++ extract z
extract (DotE x y) = extract x ++ extract y
extract (NormE x) = extract x
extract (IndE x _) = extract x
extract (VecE x) = concat $ map extract x
extract (LitE _) = []
extract (VarE x) = [x]
    
scalar env func x = do
  x <- evaluate env x
  case x of
    RealR x -> return $ RealR $ func x
    _ -> fail "Real scalar required."

numeric env func x y = do
  x <- evaluate env x
  y <- evaluate env y
  case (x, y) of
    (RealR x, RealR y) -> return $ RealR $ x `func` y
    (VectorR x, VectorR y) | length x == length y && all isReal x && all isReal y -> return $ VectorR $ zipWith (\(RealR x) (RealR y) -> RealR $ x `func` y) x y
    _ -> fail "Numeric type required."

isReal (RealR _) = True
isReal _ = False

{-# INLINE gaussian #-}
gaussian :: Double -> Double -> Double -> Double
gaussian !x !mu !variance = exp $ - ((x - mu) ** 2) / (2 * variance)

type PhaseSpace = UV.Vector Double

{- A flat vector is used to represent rank-n phase spaces; the index mapping (and length) change. -}

constructPhaseSpace :: [Value] -> (PhaseSpace, V.Vector Repr)
constructPhaseSpace vs = 
  let poss = V.fromList $ map (\x -> if length x == 1 then head x else VectorR x) $ combinations $ map rankTransform vs
      dim = V.length poss
  in ((\x -> let s = UV.sum x in UV.map (flip (/) s) x) $ UV.replicate dim 1.0, poss)

combinations :: [[a]] -> [[a]]
combinations [] = []
combinations (x:[]) = map return x
combinations (x:xs) = let !r = combinations xs in concat (map (\i -> map ((:) i) r) x)
      
rankTransform (FiniteSet o) = map FiniteSetR [0 .. length o - 1]
rankTransform (BoundedReal x y) = map (\n -> RealR $ x + (n * (y - x) / granularity)) [0 .. granularity]
rankTransform (Vector _) = error "Recursive base types are not supported."

type InternalState = M.HashMap String (MVar PhaseSpace, V.Vector Repr)

compile :: Model -> IO (Either String InternalState)
compile (Model types functions objects observables) = 
  let defined = M.keys types in
    if not (all (\x -> all (flip elem defined) x) (map (map fst . fst) $ M.elems functions)) || not (all (flip elem defined) (M.elems objects)) then return $ fail "Undefined type." else do
      r <- flip mapM (M.toList objects) $ \(k, v) -> 
        let (ps, pr) = constructPhaseSpace $ (\(Just x) -> x) $ M.lookup v types in do
          v <- newMVar ps
          return $ (k, (v, pr))
      return $ return $ M.fromList r

foldApprox :: PhaseSpace -> V.Vector Repr -> Repr
-- foldApprox s p = p V.! (UV.maxIndex s)
foldApprox s p = V.foldl radd (RealR 0) $ V.imap (\i x -> scale (p V.! i) x) $ UV.convert s

scale (RealR x) y = RealR $ x * y
scale (VectorR x) y = VectorR $ map (flip scale y) x
scale _ _ = error "Type mismatch."

(RealR x) `radd` (RealR y) = RealR $ x + y
(VectorR x) `radd` (VectorR y) = VectorR $ map (uncurry radd) $ zip x y
_ `radd` _ = error "Type mismatch."

reprDelta (RealR x) (RealR y) s = gaussian x y s
reprDelta (FiniteSetR x) (FiniteSetR y) s = error "Unsupported result type."
reprDelta (VectorR x) (VectorR y) s = sum $ map (\(x, y) -> reprDelta x y s) $ zip x y
reprDelta _ _ _ = error "Type mismatch."

update :: Model -> InternalState -> Observation -> IO Result
update (Model _ functions _ _) state (Observation fn objs val sig) =
  case (M.lookup fn functions, sequence $ map (flip M.lookup state) objs) of
    (Just (bindings, f), Just objv) -> do
      obji <- sequence $ map (\(n, (d, p)) -> readMVar d >>| \d -> (n, foldApprox d p)) $ zip (map snd bindings) objv
      flip mapM (zip (map snd bindings) objv) $ \(name, (var, poss)) ->
        let others = filter ((/=) name . fst) obji in do
          modifyMVar_ var $ \ps ->
            let !x = UV.imap (\i x -> x * reprDelta val ((\(Right x) -> x) $ evaluate (M.fromList $ (name, poss V.! i):others) f) sig) ps
                !s = UV.sum x
            in return $ UV.map (flip (/) s) x
      return Updated
    _ -> return $ Error "Function or object not found."
