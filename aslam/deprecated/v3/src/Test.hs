{- just examples -}

module Test where

import Protocol
import Symbolic.Syntax
import Symbolic.CPPGen

import qualified Data.HashMap.Strict as M

-- (IDENTITY)

identity = Model {
  modelFunctions = M.fromList [("id", Function (M.fromList [("x", BoundedRealT 0 1)]) (VarE "x"))],
  modelObjects = M.fromList [("x", BoundedRealT 0 1)],
  modelGranularity = 500
}

identityObs = Observation "id" (M.fromList [("x", Protocol.Object "x")]) (RealR 0.5) 0.1

-- (LIN EQ)

{- f1 : x + y + z, f2 : x + y - z, f3 : x - y + z -}

linearSys = Model {
  modelFunctions = M.fromList [
    ("f1", Function (M.fromList [("v", VectorT [BoundedRealT 0 5, BoundedRealT 0 5, BoundedRealT 0 5])]) (AddE (IndE (VarE "v") 0) (AddE (IndE (VarE "v") 1) (IndE (VarE "v") 2)))),
    ("f2", Function (M.fromList [("v", VectorT [BoundedRealT 0 5, BoundedRealT 0 5, BoundedRealT 0 5])]) (AddE (IndE (VarE "v") 0) (SubE (IndE (VarE "v") 1) (IndE (VarE "v") 2)))),
    ("f3", Function (M.fromList [("v", VectorT [BoundedRealT 0 5, BoundedRealT 0 5, BoundedRealT 0 5])]) (AddE (IndE (VarE "v") 0) (SubE (IndE (VarE "v") 2) (IndE (VarE "v") 1))))
  ],
  modelObjects = M.fromList [("v", VectorT [BoundedRealT 0 5, BoundedRealT 0 5, BoundedRealT 0 5])],
  modelGranularity = 200
}

-- x = y = 1, z = 0

linearSysO1 = Observation "f1" (M.fromList [("v", Protocol.Object "v")]) (RealR 2) 0.1
linearSysO2 = Observation "f2" (M.fromList [("v", Protocol.Object "v")]) (RealR 2) 0.1
linearSysO3 = Observation "f3" (M.fromList [("v", Protocol.Object "v")]) (RealR 0) 0.1

-- (NONLIN EQ)

-- x^2 + 3x + 2 - y = 0, 2x + 3 - y = 0

nonlinearSys = Model {
  modelFunctions = M.fromList [
    ("f1", Function (M.fromList [("v", VectorT [BoundedRealT (-5) 5, BoundedRealT (-5) 5])]) (SubE (AddE (AddE (PowE (IndE (VarE "v") 0) (LitE (RealR 2.0))) (MulE (IndE (VarE ("v")) 0) (LitE (RealR 3)))) (LitE (RealR 2.0))) (IndE (VarE "v") 1))),
    ("f2", Function (M.fromList [("v", VectorT [BoundedRealT (-5) 5, BoundedRealT (-5) 5])]) (SubE (AddE (MulE (IndE (VarE "v") 0) (LitE (RealR 2))) (LitE (RealR 3))) (IndE (VarE "v") 1)))
  ],
  modelObjects = M.fromList [("v", VectorT [BoundedRealT (-5) 5, BoundedRealT (-5) 5])],
  modelGranularity = 500
}

nonlinearSysO1 = Observation "f1" (M.fromList [("v", Protocol.Object "v")]) (RealR 0) 0.1
nonlinearSysO2 = Observation "f2" (M.fromList [("v", Protocol.Object "v")]) (RealR 0) 0.1
