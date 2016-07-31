module Main where

import Protocol
import Symbolic.Syntax
import Templating

$(deriveConstructors ''Repr)
$(deriveConstructors ''Type)
$(deriveConstructors ''Expr)

$(deriveConstructors ''Function)
$(deriveConstructors ''Model)
$(deriveConstructors ''Parameter)
$(deriveConstructors ''Object)
$(deriveConstructors ''Observation)
$(deriveConstructors ''Request)
$(deriveConstructors ''Action)

main = return ()
