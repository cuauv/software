module Backends.Interpreter where

import           Control.Concurrent
import           Control.DeepSeq
import qualified Data.HashMap.Strict  as M
import           Data.Maybe
import           Data.Random.Normal
import qualified Data.Set             as S
import qualified Data.Text            as T
import qualified Data.Vector          as V
import           GHC.Generics
import           System.Random

import           Execution.Eval
import           Execution.PhaseSpace
import           Execution.Typecheck
import           Protocol.DSL
import           Utility.Control

interpreter :: BackendT
interpreter = BackendT $ do
  state <- newMVar emptyState
  return (handle state, return ())

data State = State {
  stateTypes :: M.HashMap T.Text Type,
  stateVals  :: M.HashMap T.Text Val
} deriving (Show, Eq, Generic)

instance NFData State

data Val =
  Const Expr |
  Var (PhaseSpace Expr)

  deriving (Show, Eq, Generic)

instance NFData Val

emptyState = State M.empty M.empty

handle :: MVar State -> Decl -> IO Resp
handle s (ObsConD expr func resample) = modifyMVar s $ \s@(State types vals) -> do
  let exprTy = typecheck types expr
      funcTy = typecheck types func
  case (exprTy, funcTy) of
    (Right exprTy, Right (LamT tys RealT)) | tys == V.singleton exprTy -> do
      let vs = V.fromList $ S.toList $ vars expr
      ps <- flip mapM vs $ \v ->
        case vals M.! v of
          Const exp -> return (v, Const exp)
          Var space -> do
            let othV = V.filter ((/=) v) vs
                othR = transientEnv vals othV
                updS = chain (\act -> V.sum $ V.map (\(oth, pr) -> pr * (unlift $ eval (M.insert v act oth) $ AppE func $ V.singleton expr)) $ unPhaseSpace othR) space
            return (v, Var $ autoResample resample $ normalize updS)
      return $!! (State types (V.foldl' (\env (x, y) -> M.insert x y env) vals ps), RespSuccess)
    (Right _, Right _) -> return (s, RespError "Constraint forall . Expr :: a, Func :: a -> RealT not satisfied.")
    (Left er, _) -> return (s, RespError er)
    (_, Left er) -> return (s, RespError er)
handle s (ObsRelD expr delt uncer resample) = modifyMVar s $ \s@(State types vals) -> do
  case (expr, typecheck types delt) of
    (VarE v, Right (VecT tys)) | V.all ((==) RealT) tys -> do
      let (Var ps) = vals M.! v
          delt' = unlift delt :: V.Vector Double
      let np = (V.zipWith (+) delt' . unlift) |<< ps
      ns <- V.mapM (\(x, p) -> V.mapM (\x -> normalIO' (0, uncer) >>| (+) x) x >>| flip (,) p) $ unPhaseSpace np
      let nv = Var $ autoResample resample $ PhaseSpace $ V.map (\(x, p) -> (lift x, p)) ns
      return $!! (State types (M.insert v nv vals), RespSuccess)
    _ -> return (s, RespError "Can only apply relative observations to and with vector expressions.")
handle s (ConD name expr) = modifyMVar s $ \s@(State types vals) -> do
  case typecheck types expr of
    Right t -> return (s { stateTypes = M.insert name t types, stateVals = M.insert name (Const expr) vals }, RespSuccess)
    Left er -> return (s, RespError er)
handle s (VarD name spec) = do
  let phaseSpace = normalize $ toPhaseSpace spec
      varType = typecheck M.empty $ fst $ V.head $ unPhaseSpace phaseSpace
  phaseSpace `deepseq` case varType of
    Right t -> modifyMVar s (\s@(State types vals) -> return (s { stateTypes = M.insert name t types, stateVals = M.insert name (Var phaseSpace) vals }, RespSuccess))
    Left er -> return $ RespError er
handle s (EvalD expr etype) = modifyMVar s $ \s@(State types vals) -> do
  case typecheck types expr of
    Right vtype -> do
      let r = flip eval expr |<< transientEnv vals (V.fromList $ S.toList $ vars expr)
      q <- case etype of
                Argmax ->
                  return $ case argmax r of
                    Just (x, y) -> RespExpr $ lift $ V.fromList [lift x, lift y]
                    Nothing     -> RespError "Impossible!"
                Prob exp -> return $ RespExpr $ lift $ fromMaybe 0 $ snd |<< argmax (predicate ((==) exp) r)
                WeightedMean ->
                  let (PhaseSpace r') = r
                      wmean = case vtype of
                        RealT  -> lift $ V.sum $ V.map (\(x, p) -> p * unlift x) r'
                        VecT _ -> lift $ V.foldl (V.zipWith (+)) (V.replicate 10 0) $ V.map (\(x, p) -> V.map ((*) p) $ unlift x) r'
                        _ -> lift $ (-1 :: Double)
                  in return $ RespExpr wmean
                Sample n -> do
                  let v = unPhaseSpace r
                  inds <- V.replicateM n $ randomRIO (0, V.length v - 1)
                  return $ RespPhaseSpace $ PhaseSpace $ V.map ((V.!) v) inds
      return (s, q)
    Left er -> return (s, RespError er)

transientEnv :: M.HashMap T.Text Val -> V.Vector T.Text -> PhaseSpace EnvR
transientEnv s vs = (V.foldl' (\e (x, y) -> M.insert x y e) M.empty . V.zip vs) |<< fuseAll (V.map (\v -> case s M.! v of (Var ps) -> ps; Const e -> return e) vs)
{-# INLINE transientEnv #-}

multPS vecs = if
  | V.length vecs <= 1 -> V.map (lift . V.singleton) $ V.head vecs
  | otherwise ->
      let h = V.head vecs
          r = multPS $ V.tail vecs in
      concatV $ V.map (\x -> V.map (lift . V.cons x . unlift) r) h

toPhaseSpace (IntGrid x) = apriori $ multPS $ V.map (\(s, e) -> V.map lift $ V.enumFromTo s e) x
toPhaseSpace (RealGrid x n) = apriori $ multPS $ V.map (\(s, e) -> let step = ((e - s) / fromIntegral n) in V.map (\n -> s + (step * (0.5 + fromIntegral n))) $ V.enumFromTo 0 (n - 1)) x
toPhaseSpace (DirectPS x) = x

vars :: Expr -> S.Set T.Text
vars (VarE x) = S.singleton x
vars (VecE x) = V.foldl S.union S.empty $ V.map vars x
vars (ObjE x) = V.foldl S.union S.empty $ V.map vars $ V.fromList $ M.elems x
vars (AppE x y) = V.foldl S.union (vars x) (V.map vars y)
vars (LamE x y) = S.filter (not . flip V.elem (V.map fst x)) $ vars y
vars (LetE x y z) = S.filter ((/=) x) $ (vars y `S.union` vars z)
vars (IndE x _) = vars x
vars (MemE x _) = vars x
vars _ = S.empty
{-# INLINE vars #-}

autoResample :: Maybe Resample -> PhaseSpace a -> PhaseSpace a
autoResample Nothing space = space
autoResample (Just Transform) space = resample space
{-# INLINE autoResample #-}

resample space =
  let (PhaseSpace x) = normalize space
      cumulative = cumulativeSum x
      num = fromIntegral $ V.length x
      inds = V.map (flip (/) num) $ V.enumFromTo 0 (num - 1)
      res  = flip V.map inds $ \pos -> let (selX, _) = V.head $ V.filter (\x -> snd x > pos) cumulative in (selX, 1 / num)
  in PhaseSpace res
{-# INLINE resample #-}

cumulativeSum = fst . V.foldl (\(acc, s) (x, y) -> let ns = y + s in let ne = (x, ns) in (V.snoc acc ne, ns)) (V.empty, 0)
{-# INLINE cumulativeSum #-}
