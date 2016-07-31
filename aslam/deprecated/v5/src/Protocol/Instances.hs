module Protocol.Instances where

import           Control.DeepSeq

import           Protocol.DSL
import           Utility

type PhaseSpaceE = PhaseSpace Expr

instance NFData BuiltIn
instance NFData Expr
instance NFData Type
instance NFData PhaseSpaceE
instance NFData Resp
instance NFData VarSpec
instance NFData EvalType
instance NFData Resample

$(deriveSerialization ''BuiltIn)
$(deriveSerialization ''Expr)
$(deriveSerialization ''Type)
$(deriveSerialization ''PhaseSpaceE)
$(deriveSerialization ''Decl)
$(deriveSerialization ''Resp)
$(deriveSerialization ''VarSpec)
$(deriveSerialization ''EvalType)
$(deriveSerialization ''Resample)
