module Execution.Typecheck (typecheck) where

import qualified Data.HashMap.Strict as M
import qualified Data.Set            as S
import qualified Data.Text           as T
import qualified Data.Vector         as V

import           Protocol.DSL
import           Utility.Control

typecheck :: EnvT -> Expr -> Either T.Text Type
typecheck _ (BoolE _) = Right BoolT
typecheck _ (IntE _) = Right IntT
typecheck _ (RealE _) = Right RealT
typecheck _ (EnumE x) = Right (EnumT $ S.singleton x)
typecheck env (VecE x) = VecT |<< mapM (typecheck env) x
typecheck env (ObjE x) = ObjT |<< mapM (typecheck env) x
typecheck env (MemE x y) =
  case typecheck env x of
    Right (ObjT z) | y `M.member` z -> Right $ z M.! y
    Right _ -> Left "Key not in object or not an object."
    Left em -> Left em
typecheck env (IndE x y) =
  case typecheck env x of
    Right (VecT z) | y < V.length z -> Right $ z V.! y
    Right _ -> Left "Vector not long enough or not a vector."
    Left em -> Left em
typecheck env (IfE x y z) = do
  xt <- typecheck env x
  yt <- typecheck env y
  zt <- typecheck env z
  case xt of
    BoolT | yt == zt -> Right yt
    _ -> Left "Conditional non-boolean or non-equivalent expression types."
typecheck env (CaseE cs) =
  case mapM (\(x, y) -> typecheck env x >>= \x -> typecheck env y >>| \y -> (x, y)) cs of
    Right ts | V.all ((==) BoolT) (V.map fst ts) -> let rty = snd (V.head ts) in if V.all ((==) rty) (V.map snd ts) then Right rty else Left "Inconsistent case expression return types."
    Right _ -> Left "Not all case expression predicates were booleans."
    Left em -> Left em
typecheck _ (BuiltInE Add) = Right $ LamT (V.fromList [RealT, RealT]) RealT
typecheck _ (BuiltInE Sub) = Right $ LamT (V.fromList [RealT, RealT]) RealT
typecheck _ (BuiltInE Div) = Right $ LamT (V.fromList [RealT, RealT]) RealT
typecheck _ (BuiltInE Mul) = Right $ LamT (V.fromList [RealT, RealT]) RealT
typecheck _ (BuiltInE Pow) = Right $ LamT (V.fromList [RealT, RealT]) RealT
typecheck _ (BuiltInE Mod) = Right $ LamT (V.fromList [RealT, RealT]) RealT
typecheck _ (BuiltInE Neg) = Right $ LamT (V.fromList [RealT]) RealT
typecheck _ (BuiltInE Abs) = Right $ LamT (V.fromList [RealT]) RealT
typecheck _ (BuiltInE Gt) = Right $ LamT (V.fromList [RealT, RealT]) BoolT
typecheck _ (BuiltInE Ge) = Right $ LamT (V.fromList [RealT, RealT]) BoolT
typecheck _ (BuiltInE Eq) = Right $ LamT (V.fromList [RealT, RealT]) BoolT
typecheck _ (BuiltInE Ne) = Right $ LamT (V.fromList [RealT, RealT]) BoolT
typecheck _ (BuiltInE Le) = Right $ LamT (V.fromList [RealT, RealT]) BoolT
typecheck _ (BuiltInE Lt) = Right $ LamT (V.fromList [RealT, RealT]) BoolT
typecheck _ (BuiltInE Sin) = Right $ LamT (V.fromList [RealT]) RealT
typecheck _ (BuiltInE Cos) = Right $ LamT (V.fromList [RealT]) RealT
typecheck _ (BuiltInE Tan) = Right $ LamT (V.fromList [RealT]) RealT
typecheck _ (BuiltInE Asin) = Right $ LamT (V.fromList [RealT]) RealT
typecheck _ (BuiltInE Acos) = Right $ LamT (V.fromList [RealT]) RealT
typecheck _ (BuiltInE Atan) = Right $ LamT (V.fromList [RealT]) RealT
typecheck _ (BuiltInE Atan2) = Right $ LamT (V.fromList [RealT, RealT]) RealT
typecheck _ (BuiltInE Gaussian) = Right $ LamT (V.fromList [RealT, RealT, RealT]) RealT
typecheck env (LamE xs y) = do
  let nenv = V.foldl (flip $ uncurry M.insert) env xs
  rty <- typecheck nenv y
  return $ LamT (V.map snd xs) rty
typecheck env (AppE x ys) = do
  xty <- typecheck env x
  yty <- mapM (typecheck env) ys
  case xty of
    LamT argty rty | yty == argty -> Right rty
    _ -> Left "Attempted to apply a non-function or differently typed function."
typecheck env (VarE x) =
  case M.lookup x env of
    Just ty -> Right ty
    Nothing -> Left $ "Variable \"" `T.append` x `T.append` "\" not found in environment."
typecheck env (LetE x y z) =
  case typecheck env y of
    Right ty -> typecheck (M.insert x ty env) z
    Left err -> Left err
