module Protocol.API where

import           Control.Concurrent
import qualified Data.HashMap.Strict as M
import qualified Data.Text           as T
import           GHC.Generics

import           Math
import           Protocol.DSL
import qualified SHM
import           Utility

data Config = Config {
  confObjects                  :: M.HashMap T.Text Object,
  confNanomsgHost              :: Host,
  confWebsocketHost            :: Host,
  confWebsocketUpdateFrequency :: Double,
  confDefaultGrid4Range        :: Vec4 (Vec2 Double),
  confDefaultGrid4Granularity  :: Int,
  confDefaultGrid3Range        :: Vec3 (Vec2 Double),
  confDefaultGrid3Granularity  :: Int,
  confShmPushFrequency         :: Double,
  confShmPullFrequency         :: Double,
  confUncertaintyFrequency     :: Double,
  confResampleFrequency        :: Double
} deriving (Show, Eq, Generic)

data Object = Object {
  objectSpec               :: ObjectSpec,
  objectResampleFrequency  :: Maybe Double,
  objectInitialUncertainty :: Double
} deriving (Show, Eq, Generic)

type VarPair a = (SHM.Var a, SHM.Var a)

data ObjectSpec =

  Vec4 { fourPos :: Vec4 Double, fourPositionOut :: Maybe (Vec4 (VarPair Double)), fourVelocityIn :: Maybe (Vec4 (VarPair Double)) } |
  Vec3 { threePos :: Vec3 Double, threePositionOut :: Maybe (Vec3 (VarPair Double)) }

  deriving (Show, Eq, Generic)

data WrappedVar =

  Var3 (Var (Vec3 Double)) |
  Var4 (Var (Vec4 Double))

data Req =

  ObsCon   { conFunc :: T.Text, conVars :: [T.Text], conVal :: Expr, conUncertainty :: Double } |
  Eval     { evalFunc :: T.Text, evalVars :: [T.Text], evalType :: EvalType }

  deriving (Show, Eq, Generic)

data Res =

  ROK |
  RErr T.Text |
  RExpr Expr

  deriving (Show, Eq, Generic)

data EvalType = Argmax | WeightedMean | Prob Expr deriving (Show, Eq, Generic)

data GUIUpdate = GUIUpdate {
  updateObj   :: T.Text,
  updateSpace :: UpdateSpace
} deriving (Show, Eq, Generic)

data UpdateSpace =
  Space3 (PhaseSpace (Vec3 Double)) |
  Space4 (PhaseSpace (Vec4 Double))

  deriving (Show, Eq, Generic)

type Vec4 a = (a, a, a, a)
type Vec3 a = (a, a, a)
type Vec2 a = (a, a)
type Var a = MVar (PhaseSpace a)

$(deriveSerialization ''Config)
$(deriveSerialization ''Object)
$(deriveSerialization ''ObjectSpec)
$(deriveSerialization ''BuiltIn)
$(deriveSerialization ''Type)
$(deriveSerialization ''Expr)
$(deriveSerialization ''EvalType)
$(deriveSerialization ''Req)
$(deriveSerialization ''Res)
$(deriveSerialization ''GUIUpdate)
$(deriveSerialization ''UpdateSpace)
