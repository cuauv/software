module CodeGen where

import Types

import Control.Monad
import Data.List

evaluate :: Exp -> Either String ([Value], String)
evaluate (LitE d) = return ([UnboundedReal], show d)
evaluate (AddE x y) = arithmeticRestricted x y $ \x y t -> return (realVector $ length t, "(" ++ x ++ " + " ++ y ++ ")") -- overload in C++
evaluate (SubE x y) = arithmeticRestricted x y $ \x y t -> return (realVector $ length t, "(" ++ x ++ " - " ++ y ++ ")") -- overload in C++
evaluate (MulE x y) = arithmeticRestricted x y $ \x y t -> return (realVector $ length t, "(" ++ x ++ " * " ++ y ++ ")") -- overload in C++
evaluate (DivE x y) = arithmeticRestricted x y $ \x y t -> return (realVector $ length t, "(" ++ x ++ " / " ++ y ++ ")") -- overload in C++
evaluate (ModE x n) = scalarRestricted x $ \x _ -> return ([BoundedReal 0 (fromIntegral n)], x ++ " % " ++ show n)
evaluate (SinE x) = scalarRestricted x $ \x _ -> return (trigResT, "std::sin(" ++ x ++ ")")
evaluate (CosE x) = scalarRestricted x $ \x _ -> return (trigResT, "std::cos(" ++ x ++ ")")
evaluate (TanE x) = scalarRestricted x $ \x _ -> return (trigResT, "std::tan(" ++ x ++ ")")
evaluate (GaussianE x y z) = 
  scalarRestricted x $ \x _ ->
  scalarRestricted y $ \y _ ->
  scalarRestricted z $ \z _ -> return ([BoundedReal 0 1], "gaussian(" ++ x ++ ", " ++ y ++ ", " ++ z ++ ")")
evaluate (DotE x y) = do
  (prodt, prod) <- evaluate (MulE x y)
  return $ if length prodt == 1 then (prodt, prod) else (prodt, prod ++ "sum(" ++ x ++ ")")
evaluate (NormE x) = do
  (xt, x) <- evaluate x
  return $ if length xt == 1 then (xt, "std::abs(" ++ x ++ ")") else (xt, "norm(" ++ x ++ ")")

trigResT = [BoundedReal (-1) 1]
realVector n = replicate n UnboundedReal

scalarRestricted x f = do
  (xty, x) <- evaluate x
  if length xty /= 1 then fail "Cannot apply operation to a vector." else f x xty

-- Auxiliary evaluation helpers.
arithmeticRestricted x y f = do
  (xty, x) <- evaluate x
  (yty, y) <- evaluate y
  unless (length xty == length yty) (fail "Types irresolvable.")
  unless (all numeric xty) (fail "Cannot perform arithmetic on specified type.")
  f x y xty

numeric x = case x of
  UnboundedReal -> True
  BoundedReal _ _ -> True
  FiniteSet _ -> False

typedef name ty = "typedef " ++ ctype ty ++ " " ++ name

ctype xs = if length xs == 1 then ctype' (head xs) else "aslam_vector<" ++ (concat $ intersperse ", " $ map ctype' xs) ++ ">"

ctype' x = case x of
  UnboundedReal -> "double"
  BoundedReal _ _ -> "double"

funcdef name bindings rt body = ctype rt ++ " " ++ name ++ "(" ++ (concat $ intersperse ", " $ map (\(x, y) -> y ++ " " ++ x) bindings) ++ ") {\n" 
  ++ "return " ++ body ++ ";\n}\n"

{- RESOLUTION
 -> Determine all individual parameters (objects).
 -> Simulate across PSPACE for each using current est. value for others by calculating P(O|W)
-}

-- pspace generation
-- parameter -> dimension
