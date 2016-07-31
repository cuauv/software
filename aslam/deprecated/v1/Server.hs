module Server where

import Auxiliary
import Types
import Templating

$(deriveSerialization ''Repr)
$(deriveSerialization ''Exp)
$(deriveSerialization ''Value)
$(deriveSerialization ''Model)

$(deriveConstructors ''Repr)
$(deriveConstructors ''Exp)
$(deriveConstructors ''Value)
$(deriveConstructors ''Model)
