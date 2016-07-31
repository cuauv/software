module Types where

import qualified Data.HashMap.Strict as M

{- Currently, only rank-1 tensors (vectors) supported. -}

data Exp = 
  {- Basic Arithmetic -}
  AddE Exp Exp |
  SubE Exp Exp |
  MulE Exp Exp |
  DivE Exp Exp |
  ModE Exp Rational |
  {- Basic Trigonometry -}
  SinE Exp |
  CosE Exp |
  TanE Exp |
  {- Auxiliary Math -}
  GaussianE Exp Exp Exp | -- x mu sigma
  {- Vector Operations -}
  DotE Exp Exp |
  NormE Exp |
  IndE Exp Int | -- Indexing.
  VecE [Exp] | -- Make vector. IndE (VecE [x]) 0 == x
  {- Literals -}
  LitE Double |
  {- Variables -}
  VarE String | -- Must have been bound in bindings; no globals.
  {- Functions -}
  AppE String [Exp] -- Functions must resolve to a DAG, no recursion. Will fail if types do not match. !! TODO
  deriving (Eq, Show)

data Repr =
  RealR !Double |
  FiniteSetR !Int |
  VectorR ![Repr] deriving (Show) -- for observations

data Value = BoundedReal { brMin :: Double, brMax :: Double } | FiniteSet { fsOptions :: [String] } | Vector [Value] deriving (Eq, Show)

data Model = Model {
  modelTypes :: M.HashMap String [Value],
  modelFunctions :: M.HashMap String ([(String, String)], Exp),
  modelObjects :: M.HashMap String String,
  modelObservables :: [(String, [String])] -- (function, parameters)
}

data Observation = Observation { observationFunction :: String, observationObjects :: [String], observationValue :: Repr, observationSigma :: Double }

data Result = 
  Updated |
  Error String deriving (Show)

{- MOVE TO CLIENT LIBS - left for template -}

{-
data Declaration = 
  TypeDec { typedecName :: String, typedecValue :: [Value] } |
  FunctionDec { functionName :: String, functionBindings :: [(String, String)], functionBody :: Exp } |
  ObjectDec { objectName :: String, objectType :: String } |
  ObservableDec { observableFunction :: String, observableParameters :: [String] }
  deriving (Show)

-}
