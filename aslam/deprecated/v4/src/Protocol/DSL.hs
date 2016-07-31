module Protocol.DSL where

import           Control.DeepSeq
import qualified Data.HashMap.Strict   as M
import qualified Data.Set              as S
import qualified Data.Text             as T
import qualified Data.Vector           as V
import           GHC.Generics

import           Utility.Serialization

{- Expr -}

data BuiltIn =

  Add |
  Sub |
  Div |
  Mul |
  Pow |
  Mod |
  Neg |
  Abs |

  Gt |
  Ge |
  Eq |
  Ne |
  Le |
  Lt |

  Sin |
  Cos |
  Tan |
  Asin |
  Acos |
  Atan |
  Atan2 |

  Gaussian

  deriving (Show, Eq, Generic)

data Expr =

  BoolE Bool |
  IntE Int |
  RealE Double |
  EnumE T.Text |

  VecE (V.Vector Expr) |
  ObjE (M.HashMap T.Text Expr) |

  MemE Expr T.Text |
  IndE Expr Int |

  CaseE (V.Vector (Expr, Expr)) |
  IfE Expr Expr Expr | -- IF {} THEN {} ELSE {}

  LamE (V.Vector (T.Text, Type)) Expr |

  BuiltInE BuiltIn |

  AppE Expr (V.Vector Expr) |
  LetE T.Text Expr Expr |
  VarE T.Text

  deriving (Show, Eq, Generic)

data Type =

  BoolT |
  IntT |
  RealT |
  EnumT (S.Set T.Text) |

  VecT (V.Vector Type) |
  ObjT (M.HashMap T.Text Type) |

  LamT (V.Vector Type) Type

  deriving (Show, Eq, Generic)

newtype PhaseSpace a = PhaseSpace { unPhaseSpace :: V.Vector (a, Double) } deriving (Show, Eq, Generic)

data Decl =

  ObsConD { coExpr :: Expr, coFunc :: Expr, coResample :: Maybe Resample } |
  ObsRelD { roExpr :: Expr, roDelt :: Expr, roUncer :: Double, roResample :: Maybe Resample } |
  ConD { conVar :: T.Text, conExpr :: Expr } |
  VarD { varVar :: T.Text, varSpec :: VarSpec } |
  EvalD { evalExpr :: Expr, evalType :: EvalType }

  deriving (Show, Eq, Generic)

data Call =

  MkEnv { mkEnv :: T.Text, mkBackend :: Backend } |
  RmEnv { rmEnv :: T.Text } |
  ExecD { execEnv :: T.Text, execDecl :: Decl }

  deriving (Show, Eq, Generic)

data Backend =

  Interpreted |
  LLVMJIT

  deriving (Show, Eq, Generic)

data Resp =

  RespSuccess |
  RespError T.Text |
  RespExpr Expr |
  RespPhaseSpace (PhaseSpace Expr)

  deriving (Show, Eq, Generic)

data Resample =

  Transform

  deriving (Show, Eq, Generic)

data VarSpec =
  RealGrid (V.Vector (Double, Double)) Int |
  IntGrid (V.Vector (Int, Int)) |
  DirectPS (PhaseSpace Expr)

  deriving (Show, Eq, Generic)

data EvalType = Argmax | WeightedMean | Prob Expr | Sample Int deriving (Show, Eq, Generic)

type EnvT = M.HashMap T.Text Type

data BackendT = BackendT {
  runBackendT :: IO (Decl -> IO Resp, IO ())
}

{- Typeclasses -}

class Liftable a where
  lift   :: a -> Expr
  unlift :: Expr -> a

instance Liftable Expr where
  lift   = id
  unlift = id

instance Liftable Bool where
  lift = BoolE
  unlift (BoolE x) = x
  unlift _ = crash "Unlift attempt to bool failed."

instance Liftable Int where
  lift = IntE
  unlift (IntE x) = x
  unlift _ = crash "Unlift attempt to int failed."

instance Liftable Double where
  lift = RealE
  unlift (RealE x) = x
  unlift _ = crash "Unlift attempt to double failed."

instance Liftable T.Text where
  lift = EnumE
  unlift (EnumE x) = x
  unlift _ = crash "Unlift attempt to text failed."

instance (Liftable a) => Liftable (V.Vector a) where
  lift = VecE . V.map lift
  unlift (VecE x) = V.map unlift x
  unlift _ = crash "Unlift attempt to vector failed."

instance (Liftable a) => Liftable (M.HashMap T.Text a) where
  lift = ObjE . M.map lift
  unlift (ObjE x) = M.map unlift x
  unlift _ = crash "Unlift attempt to hashmap failed."

crash msg = error $ "ASLAM internal error: " ++ msg

type PhaseSpaceE = PhaseSpace Expr

instance NFData BuiltIn
instance NFData Expr
instance NFData Type
instance NFData PhaseSpaceE
instance NFData Resp
instance NFData VarSpec
instance NFData EvalType
instance NFData Resample
instance NFData Backend

$(deriveSerialization ''BuiltIn)
$(deriveSerialization ''Expr)
$(deriveSerialization ''Type)
$(deriveSerialization ''PhaseSpaceE)
$(deriveSerialization ''Decl)
$(deriveSerialization ''Call)
$(deriveSerialization ''Resp)
$(deriveSerialization ''VarSpec)
$(deriveSerialization ''EvalType)
$(deriveSerialization ''Resample)
$(deriveSerialization ''Backend)
