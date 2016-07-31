module Protocol where

import Auxiliary
import Symbolic.Syntax

import qualified Data.HashMap.Strict as M

data Function = Function {
  functionArguments :: M.HashMap String Type,
  functionBody :: Expr
} deriving (Show, Eq)

data Model = Model {
  modelFunctions :: M.HashMap String Function,
  modelObjects :: M.HashMap String Object,
  modelGranularity :: Int
} deriving (Show, Eq)

data Object = Object { objectType :: Type, objectTimeDecay :: Maybe Double } deriving (Eq, Show) -- time decay per second, exponential

data Parameter = PActual Repr | PObject String deriving (Show)

data Observation = Observation {
  observationFunction :: String,
  observationParameters :: M.HashMap String Parameter,
  observationValue :: Repr,
  observationUncertainty :: Double
} deriving (Show)

data Request = Request { requestModel :: Maybe Model, requestNamespace :: String, requestAction :: Action } deriving (Show)

data Action =
  Observe Observation |
  Estimate String |
  Sample String
  deriving (Show)

data Response =
  Success |
  Error String |
  Estimated Repr |
  Sampled [(Repr, Double)]
  deriving (Show)

$(deriveSerialization ''Type)
$(deriveSerialization ''Repr)
$(deriveSerialization ''Expr)
$(deriveSerialization ''Function)
$(deriveSerialization ''Object)
$(deriveSerialization ''Model)
$(deriveSerialization ''Parameter)
$(deriveSerialization ''Observation)
$(deriveSerialization ''Request)
$(deriveSerialization ''Action)
$(deriveSerialization ''Response)
