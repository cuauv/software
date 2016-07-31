{-
  Compiles symbolic expressions to Haskell functions.
  The current backend for symbolic computation; C++ code generation coming soon.
-}

{-# LANGUAGE DeriveGeneric #-}

module HaskellImpl (CompiledModel, RuntimeState, observe, estimate, loadModel, compileModel) where

import Auxiliary
import Symbolic
import Protocol

import Control.Concurrent
import Control.DeepSeq
import Control.Monad
import Data.Fixed (mod')
import Data.List
import qualified Data.HashMap.Strict as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import GHC.Generics

{- Types -}

type PhaseSpace = V.Vector Repr
type PhaseSpaceR = UV.Vector Double

data CompiledModel = CompiledModel {
  compiledFunctions :: M.HashMap String (Type, [String], M.HashMap String Repr -> Repr),
  compiledObjects :: M.HashMap String PhaseSpace
} deriving (Generic)

instance NFData CompiledModel

granularity = 200

type RuntimeState = M.HashMap String (MVar PhaseSpaceR)

{- Runtime -}

{-# INLINE normalize #-}
normalize v = let s = UV.sum v in (UV.map $ flip (/) s) v

{-# INLINE gaussian #-}
gaussian :: Double -> Double -> Double -> Double
gaussian !x !mu !variance = exp $ - ((x - mu) ** 2) / (2 * variance)

isLeft (Left _) = True
isLeft _ = False

lefts = M.map (\(Left x) -> x) . M.filter isLeft
rights = M.map (\(Right x) -> x) . M.filter (not . isLeft)

observe :: CompiledModel -> RuntimeState -> Observation -> IO (Either String ())
observe (CompiledModel funcs objs) state (Observation func params val uncert) =
  if not $ foldl (&&) True $ (M.member func funcs) : map (flip M.member objs) (M.elems $ lefts params) ++ map (flip M.member state) (M.elems $ lefts params)
    then return $ Left "Object or function not found." else
      let objv = map (\(n, b) -> (n, state M.! b, objs M.! b)) $ M.toList $ lefts params
          (_, vars, ufxn) = funcs M.! func in if not $ sort vars == sort (M.keys params) then return $ Left "Invalid arguments." else do
            initial <- mapM (\(n, v, s) -> readMVar v >>| \st -> (n, foldApprox s st)) objv
            flip mapM objv $ \(n, v, st) -> let others = filter ((/=) n . fst) initial in
              modifyMVar_ v (return . normalize . UV.imap (\i x -> x * delta (ufxn (M.union (rights params) $ M.insert n (st V.! i) $ M.fromList others)) val uncert))
            return $ return ()

delta :: Repr -> Repr -> Double -> Double
delta (RealR x) (RealR y) sig = gaussian x y sig
delta (VectorR x) (VectorR y) sig = foldl (*) 1 $ map (\(x, y) -> delta x y sig) $ zip x y
delta _ _ _ = crash "delta failed"

estimate :: CompiledModel -> RuntimeState -> String -> IO (Either String Repr)
estimate (CompiledModel _ objs) state name = 
  case (M.lookup name objs, M.lookup name state) of
    (Just space, Just objv) -> return |<< (foldApprox space |<< readMVar objv)
    _ -> return $ Left "Object not found."

loadModel :: CompiledModel -> IO RuntimeState
loadModel (CompiledModel _ objs) = 
  M.fromList |<< (sequence $ map (\(k, v) -> 
    let !x = (normalize $ UV.replicate (V.length v) 1.0) in x `deepseq` newMVar x >>| (,) k) $ M.toList objs)

compileModel :: Model -> Either String CompiledModel
compileModel (Model funcs objs) = do
  cfuncs <- M.fromList |<< (mapM (\(k, v) -> uncurry compileExpr v >>| \e -> (k, (fst e, nub $ extractExpr (snd v), snd e))) $ M.toList funcs)
  return $!! CompiledModel cfuncs $ M.map constructPhaseSpace objs

foldApprox :: PhaseSpace -> PhaseSpaceR -> Repr
foldApprox s r = s V.! (UV.maxIndex r) {- TODO: Intelligent folding. -}

constructPhaseSpace :: Type -> PhaseSpace
constructPhaseSpace = V.fromList . rankTransform

rankTransform (FiniteSet o) = map FiniteSetR o
rankTransform (BoundedReal x y) = map (\n -> RealR $ x + (n * (y - x) / granularity)) [0 .. granularity]
rankTransform (Vector ts) = map VectorR $ combinations $ map rankTransform ts

combinations [] = []
combinations (x:[]) = map return x
combinations (x:xs) = let !r = combinations xs in concat (map (\i -> map ((:) i) r) x)

extractExpr (Add x y) = extractExpr x ++ extractExpr y
extractExpr (Sub x y) = extractExpr x ++ extractExpr y
extractExpr (Mul x y) = extractExpr x ++ extractExpr y
extractExpr (Div x y) = extractExpr x ++ extractExpr y
extractExpr (Mod x y) = extractExpr x ++ extractExpr y
extractExpr (Pow x y) = extractExpr x ++ extractExpr y
extractExpr (Neg x) = extractExpr x
extractExpr (Sin x) = extractExpr x
extractExpr (Cos x) = extractExpr x
extractExpr (Tan x) = extractExpr x
extractExpr (Vec x) = concat $ map extractExpr x
extractExpr (Ind (Vec x) i) = extractExpr $ x !! i
extractExpr (Ind (Var x) _) = return x
extractExpr (Ind _ _) = crash "ind failed"
extractExpr (DoubleL _) = []
extractExpr (Let x y z) = delete x $ extractExpr y ++ extractExpr z -- shadowing?
extractExpr (Var x) = return x

{- Compilation -}

compileExpr :: M.HashMap String Type -> Expression -> Either String (Type, M.HashMap String Repr -> Repr)
compileExpr t (Add x y) = dualArith t (+) x y
compileExpr t (Sub x y) = dualArith t (-) x y
compileExpr t (Mul x y) = dualArith t (*) x y
compileExpr t (Div x y) = dualArith t (/) x y
compileExpr t (Mod x y) = dualArith t (mod') x y
compileExpr t (Pow x y) = dualArith t (**) x y
compileExpr t (Neg x) = singleArith t (\x -> -x) x
compileExpr t (Sin x) = singleArith t sin x
compileExpr t (Cos x) = singleArith t cos x
compileExpr t (Tan x) = singleArith t tan x
compileExpr t (Vec x) = do
  cmp <- sequence $ map (compileExpr t) x
  return $ (Vector $ map fst cmp, \e -> VectorR $ map (\x -> x e) $ map snd cmp)
compileExpr t (Ind x i) = do
  (xty, x) <- compileExpr t x
  case xty of
    Vector tys | i >= 0 || length tys > i -> return $ (tys !! i, \e -> (\(VectorR x) -> x !! i) (x e))
    _ -> Left "Invalid type for indexing or invalid index."
compileExpr _ (DoubleL d) = return $ (BoundedReal d d, \_ -> RealR d)
compileExpr t (Case _) = undefined
compileExpr t (Let x y z) = do
  (yty, y) <- compileExpr t y
  (zty, z) <- compileExpr (M.insert x yty t) z
  return $ (zty, \e -> z (M.insert x (y e) e))
compileExpr t (Var x) = 
  case M.lookup x t of
    Just ty -> return $ (ty, \e -> e M.! x)
    Nothing -> Left "Variable type not found."

singleArith t f x = do
  (xty, x) <- compileExpr t x
  unless (isNumeric xty) $ Left "Type must be numeric."
  return (xty, \e -> singleArithApply f (x e))

singleArithApply f (RealR x) = RealR $ f x
singleArithApply f (VectorR x) = VectorR $ map (singleArithApply f) x
singleArithApply _ _ = crash "saa failed"

dualArith t f x y = do
  (xty, x) <- compileExpr t x
  (yty, y) <- compileExpr t y
  unless (isNumeric xty && isNumeric yty) $ Left "Type must be numeric."
  unless (xty == yty) $ Left "Types do not match."
  return (xty, \e -> dualArithApply f (x e) (y e)) 

dualArithApply f (RealR x) (RealR y) = RealR $ x `f` y
dualArithApply f (VectorR x) (VectorR y) = VectorR $ map (uncurry $ dualArithApply f) $ zip x y
dualArithApply _ _ _ = crash "daa failed"

isNumeric (BoundedReal _ _) = True
isNumeric (FiniteSet _) = False
isNumeric (Vector ts) = foldl (&&) True $ map isNumeric ts

crash s = error $ "ASLAM internal error; this is a bug. Note: " ++ s
