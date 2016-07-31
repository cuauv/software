module Symbolic.Syntax where

data Repr =
  BoolR !Bool |
  RealR !Double |
  FiniteSetR !String |
  VectorR ![Repr]
  
  deriving (Show, Eq)

data Type = 
  BoolT |
  BoundedRealT { brMin :: Double, brMax :: Double } |
  FiniteSetT { fsOptions :: [String] } |
  VectorT [Type]
  
  deriving (Show, Eq)

{- 
  Expression compilation is typesafe; if it compiles, it is guaranteeed to execute without failure.
  As such, no dynamic-length vectors; indexing must be known at compile-time.
  No automatic broadcasting (like Numpy), although this can be done in client libraries quite easily. 
-}

data Expr =
  
  {- Arithmetic -}
  AddE Expr Expr |
  SubE Expr Expr |
  MulE Expr Expr |
  DivE Expr Expr |
  ModE Expr Expr |
  PowE Expr Expr |
  NegE Expr |

  {- Comparision -}
  GtE Expr Expr | {- > -}
  GeE Expr Expr | {- >= -}
  EqE Expr Expr | {- == -}
  LeE Expr Expr | {- <= -}
  LtE Expr Expr | {- < -} 
  
  {- TODO: Should these be static? -}

  {- Trigonometry -}
  SinE Expr |
  CosE Expr |
  TanE Expr |
  ATan2E Expr Expr |

  {- Rank Transforms -}
  VecE [Expr] |
  IndE Expr Int | -- Static indexing, compile-time bounds checks.

  {- Rebinning -}
  CaseE [(Expr, Expr)] | -- if a: b; elif c: d; etc. Must cover all possible cases, this is checked. Overlap prohibited.
  
  {- Variables, Bindings -}
  LetE String Expr Expr | -- let x = y in z. Not recursive. Shadows.
  VarE String | -- Compile-time checked.

  {- Literals -}
  LitE Repr

  deriving (Show, Eq)
