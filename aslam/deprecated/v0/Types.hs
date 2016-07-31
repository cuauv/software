-- Nodes: unique global identifier, type.

{- Node Types:
  
  Metatype:
  "Certain" - e.g. observed directly (~ HMM output)
  "Uncertain" - e.g. unknown, although often with initial estimates, and not directly observable states (~ HMM hidden states)
  "Uncertain" metatypes must also provide an estimation method for each of the parameters from the probability space: mean / median / mode. (mean corresponds to a weighted centroid, mode to the highest-probability value, and median to the value at "0.5" probability on the CDF)  

  Discrete type:
  Some variant of Math.Tensor:
    Math.Tensor ~ [Dimension]; Dimension := [Value]; Value := Discrete [OPTIONS] | Bounded MIN MAX | Unbounded | Modulo Double
  Note that "uncertain" transforms a tensor of dimension d into a tensor of rank d (a 3-vector into a rank-3 tensor, e.g.; with position) in internal representation. 

  Put "uncertain" in the values? Good - seperate "position" from its associated nodes. -- uncertain "type", not value.

  Specify "derivations" for certain types.

  ASLAM.Function

    [Parent Node]

  Exp := VarExp (name ~ string) | Mult Exp Exp | Div Exp Exp | Pow Exp Exp | Plus Exp Exp | Minus Exp Exp | Neg Exp | LitExp Double

  All obs have gaussian: x - obs, mu - prior, stddev: specified.
-}

data Schema = Schema [Node]

data Node = Node { nodeName :: String, nodeType :: NodeType }

data NodeType = Certain (Tensor DerivedValue) | Uncertain (Tensor UnknownValue)

data EstimationMethod = Mean | Median | Mode

data Tensor a = Tensor [Dimension a]

data Dimension a = Dimension [a]

data DerivedValue = DerivedValue Value Exp
data UnknownValue = UnknownValue Value EstimationMethod

data Value = Discrete {- TODO -} | Bounded { vbMin :: Double, vbMax :: Double } | Unbounded | Modulo Double

{- Limited Symbolic Algebra -}

data Exp = Lit Double | Var String Int Int | Mul Exp Exp | Div Exp Exp | Pow Exp Exp | Add Exp Exp | Sub Exp Exp | Neg Exp | Sin Exp | Cos Exp

class PP a where
  pp :: a -> String

instance PP Exp where
  pp (Lit d) = show d
  pp (Var n d i) = n ++ "[" ++ show d ++ "," ++ show i ++ "]"
  pp (Mul x y) = "(" ++ pp x ++ " * " ++ pp y ++ ")"
  pp (Div x y) = "(" ++ pp x ++ " / " ++ pp y ++ ")"
  pp (Pow x y) = "(" ++ pp x ++ " ^" ++ pp y ++ ")"
  pp (Add x y) = "(" ++ pp x ++ " + " ++ pp y ++ ")"
  pp (Sub x y) = "(" ++ pp x ++ " - " ++ pp y ++ ")"
  pp (Neg x) = "-" ++ pp x
  pp (Sin x) = "sin(" ++ pp x ++ ")"
  pp (Cos x) = "cos(" ++ pp x ++ ")"
