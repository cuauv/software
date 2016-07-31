module Backends.CPPGen where

import qualified Data.HashMap.Strict as M
import qualified Data.Set            as S
import qualified Data.Text           as T
import qualified Data.Vector         as V

import           Protocol.DSL

modelGen ∷ Model → (T.Text, T.Text)
modelGen (Model cons vars) =
  let cList = M.toList cons
      vList = M.toList vars

      fwdDecs = map (\(k, (ty, _)) -> fwdDecCon k ty) cList ++ map (\(k, (ty, _)) -> fwdDecVar k ty) vList
      stdDecs = map (\(k, (ty, e)) -> decCon k ty e) cList ++ map (\(k, (ty, s)) -> decVar k ty s) vList

      headerFile =  T.unlines [
                      "#include \"aslam.h\"",
                      "\n/* FORWARD DECLARATIONS */\n",
                      T.unlines fwdDecs,
                      "\n/* EOF */"
                    ]
      modelFile  =  T.unlines [
                      "#include \"model.h\"",
                      "\n/* DECLARATIONS */\n",
                      T.unlines stdDecs,
                      "\n/* EOF */"
                    ]

  in (headerFile, modelFile)

fwdDecCon ∷ T.Text → Type → T.Text
fwdDecCon name ty = T.concat ["extern ", typeGen ty, " ", name, ";"]

fwdDecVar ∷ T.Text → Type → T.Text
fwdDecVar name ty = T.concat ["extern Var<", typeGen ty, "> ", name, ";"]

decCon ∷ T.Text → Type → Expr → T.Text
decCon name ty expr = T.concat [typeGen ty, " ", name, " = ", exprGen expr, ";"]

decVar ∷ T.Text → Type → VarSpec → T.Text
decVar name ty spec = T.concat ["Var<", typeGen ty, "> ", name, " = ", specGen ty spec, ";"]

builtinGen ∷ BuiltIn → T.Text
builtinGen = T.toLower . T.append "builtin_" . showT
{- Templated -}

{- TODO: Autogen application functions. -}

exprGen ∷ Expr → T.Text
exprGen (BoolE b) = if b then "true" else "false"
exprGen (IntE i) = showT i
exprGen (RealE r) = showT r
exprGen (VecE es) = T.concat ["make_tuple(", commaJoin exprGen es, ")"]
exprGen (IndE e i) = T.concat ["get<", showT i, ">(", exprGen e, ")"]
exprGen (IfE x y z) = T.concat [exprGen x, " ? ", exprGen y, " : ", exprGen z]
exprGen (BuiltInE b) = builtinGen b
exprGen (AppE f es) = T.concat [exprGen f, "(", commaJoin exprGen es, ")"]
exprGen (LamE xs (re, rty)) = T.concat ["[](", commaJoin (\(x, y) -> T.concat [typeGen y, " ", x]) xs, ") -> ", typeGen rty, " { return (", exprGen re, "); }"]
-- exprGen (LetE x y z) = undefined -- Replace all? Maybe optimized away.
exprGen (VarE x) = x
exprGen (ConE x) = x

typeGen ∷ Type → T.Text
typeGen BoolT = "bool"
typeGen IntT = "int"
typeGen RealT = "double"
typeGen (EnumT _) = "int"
typeGen (VecT tys) = T.concat ["tuple<", commaJoin typeGen tys, ">"]
typeGen (LamT tys rty) = T.concat ["function<", typeGen rty, "(", commaJoin typeGen tys, ")", ">"]

psGen ∷ PhaseSpace Expr → T.Text
psGen (PhaseSpace ps) = undefined

specGen ∷ Type → VarSpec → T.Text
specGen ty spec = T.concat ["Var<", typeGen ty, ">( {} )"]

showT ∷ forall a . Show a ⇒ a → T.Text
showT = T.pack . show

commaJoin ∷ forall a . (a → T.Text) → [a] → T.Text
commaJoin func = T.intercalate ", " . map func

example ∷ Model
example = Model {
  modelConstants = M.fromList [
    ("plusTwo", (LamT [RealT] RealT, LamE [("val", RealT)] (VarE "val", RealT)))
    ],
  modelVariables = M.fromList [
    ("x", (RealT, RealGrid (V.singleton (-10, 10)) 20))
  ]
  }
