module Execution.Eval (EnvR, eval) where

import           Data.Fixed          (mod')
import qualified Data.HashMap.Strict as M
import qualified Data.Text           as T
import qualified Data.Vector         as V

import           Protocol.DSL

{- Canonical Discrete Evaluation -- no closures. -}

type EnvR = M.HashMap T.Text Expr

eval :: EnvR -> Expr -> Expr
eval _ x@(BoolE _) = x
eval _ x@(IntE _) = x
eval _ x@(RealE _) = x
eval _ x@(EnumE _) = x
eval env (VecE x) = VecE $ V.map (eval env) x
eval env (ObjE x) = ObjE $ M.map (eval env) x
eval env (MemE x y) = (unlift $ eval env x) M.! y
eval env (IndE x y) = (unlift $ eval env x) V.! y
eval env (IfE x y z) = if unlift $ eval env x then eval env y else eval env z
eval env (CaseE xs) = snd $ V.head $ V.filter (unlift . eval env . fst) xs
eval _ x@(LamE _ _) = x
eval _ x@(BuiltInE _) = x
eval env (AppE x y) =
  let args = V.map (eval env) y
      expr = eval env x in
  case expr of
    LamE x y -> eval (V.foldl (\env (x, y) -> M.insert x y env) env $ V.zip (V.map fst x) args) y
    BuiltInE x ->
      case x of
        Add -> dualA (+) args
        Sub -> dualA (-) args
        Div -> dualA (/) args
        Mul -> dualA (*) args
        Pow -> dualA (**) args
        Mod -> dualA (mod') args
        Neg -> unaryA negate args
        Abs -> unaryA abs args
        Gt -> dualC (>) args
        Ge -> dualC (>=) args
        Eq -> lift $ args V.! 0 == args V.! 1
        Ne -> dualC (/=) args
        Le -> dualC (<=) args
        Lt -> dualC (<) args
        Sin -> unaryA sin args
        Cos -> unaryA cos args
        Tan -> unaryA tan args
        Asin -> unaryA asin args
        Acos -> unaryA acos args
        Atan -> unaryA atan args
        Atan2 -> dualA atan2 args
        Gaussian -> ternaryA gaussian args
    _ -> crash "Attempted to apply a non-applicable expression."
eval env (LetE x y z) = eval (M.insert x (eval env y) env) z
eval env (VarE x) = env M.! x
{-# INLINE eval #-}

dualC :: (Double -> Double -> Bool) -> V.Vector Expr -> Expr
dualC = dual
{-# INLINE dualC #-}

ternaryA :: (Double -> Double -> Double -> Double) -> V.Vector Expr -> Expr
ternaryA = ternary
{-# INLINE ternaryA #-}

dualA :: (Double -> Double -> Double) -> V.Vector Expr -> Expr
dualA = dual
{-# INLINE dualA #-}

unaryA :: (Double -> Double) -> V.Vector Expr -> Expr
unaryA = unary
{-# INLINE unaryA #-}

ternary x y = lift $ x (unlift $ y V.! 0) (unlift $ y V.! 1) (unlift $ y V.! 2)
{-# INLINE ternary #-}

dual x y = lift $ x (unlift $ y V.! 0) (unlift $ y V.! 1)
{-# INLINE dual #-}

unary x y = lift $ x (unlift $ y V.! 0)
{-# INLINE unary #-}

gaussian :: Double -> Double -> Double -> Double
gaussian !x !mu !variance = exp $ - ((x - mu) ** 2) / (2 * variance)
{-# INLINE gaussian #-}
