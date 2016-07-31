module Protocol where

import Auxiliary
import Symbolic

import qualified Data.HashMap.Strict as M

data Model = Model {
  modelFunctions :: M.HashMap String (M.HashMap String Type, Expression),
  modelObjects :: M.HashMap String Type
} deriving (Show)

data Observation = Observation { 
  observationFunction :: String, 
  observationParameters :: M.HashMap String (Either String Repr),
  observationValue :: Repr,
  observationUncertainty :: Double
} deriving (Show)

data Request = WithSchema { wsSchema :: String, wsRequest :: AtomicRequest } deriving (Show)

data AtomicRequest =
  Load Model |
  Observe Observation |
  Estimate String deriving (Show)

data Response =
  Success |
  Error String |
  Estimated Repr deriving (Show)

$(deriveSerialization ''Model)
$(deriveSerialization ''Observation)
$(deriveSerialization ''Request)
$(deriveSerialization ''AtomicRequest)
$(deriveSerialization ''Response)
$(deriveSerialization ''Expression)
$(deriveSerialization ''Repr)
$(deriveSerialization ''Type)
