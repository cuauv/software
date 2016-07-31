module Auxiliary where

import qualified Data.Aeson.TH as A
import Data.Char

defaultJSONOptions = A.defaultOptions {
  A.fieldLabelModifier = map toLower . dropWhile isLower, -- e.g. <datatype><Fieldname> as record accessor
  A.constructorTagModifier = map toLower,
  A.omitNothingFields = True,
  A.sumEncoding = A.ObjectWithSingleField
}

(>>|) x y = fmap y x
(|<<) x y = fmap x y

deriveSerialization = A.deriveJSON defaultJSONOptions
