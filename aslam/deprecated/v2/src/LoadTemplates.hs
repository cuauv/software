module LoadTemplates where

import Protocol
import Symbolic
import Templating

$(deriveConstructors ''Repr)
$(deriveConstructors ''Type)
$(deriveConstructors ''Expression)
$(deriveConstructors ''Model)
$(deriveConstructors ''Observation)
$(deriveConstructors ''Request)
$(deriveConstructors ''AtomicRequest)
