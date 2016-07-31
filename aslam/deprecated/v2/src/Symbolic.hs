{-  
  ASLAM Symbolic Computation.
  Simple functional-style expression syntax expressible in JSON.
-}

{-# LANGUAGE DeriveGeneric #-}

module Symbolic where

import Control.DeepSeq
import Data.List
import GHC.Generics

data Repr =
  RealR !Double |
  FiniteSetR !String |
  VectorR ![Repr] deriving (Eq, Show)

data Type = 
  BoundedReal { brMin :: Double, brMax :: Double } |
  FiniteSet { fsOptions :: [String] } |
  Vector [Type] deriving (Show, Generic)

instance Eq Type where
  (BoundedReal _ _) == (BoundedReal _ _) = True
  FiniteSet x == FiniteSet y = sort x == sort y
  Vector tx == Vector ty = tx == ty
  _ == _ = False

instance NFData Type
  
{- 
  Expression compilation is typesafe but some automatic broadcast rules are applied (similiar to NumPy).
  In particular, most function applications will be automatically mapped if the two arguments differ by one rank (or over one argument).
  See documentation for details. 
-}

data Expression = 
  
  {- Arithmetic -}
  Add Expression Expression |
  Sub Expression Expression |
  Mul Expression Expression |
  Div Expression Expression |
  Mod Expression Expression |
  Pow Expression Expression |
  Neg Expression |
  
  {- Trigonometry -}
  Sin Expression |
  Cos Expression |
  Tan Expression |
  
  {- Rank Transforms -}
  Vec [Expression] |
  Ind Expression Int | -- Currently static indexing; this allows us to avoid runtime bounds checks. 
  
  {- Literals -}
  DoubleL Double |
  
  {- FiniteSet Manipulation -}
  Case [(String, Expression)] | -- Must cover all cases; this is checked.

  {- Variables, Bindings -}
  Let String Expression Expression | -- let x = y in z. Not recursive. Shadows.
  Var String -- Bindings: function arguments, whatever was "let" bound. No globals. Types must be specified.
  
  deriving (Eq, Show)
