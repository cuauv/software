module Utility.Control where

import           Control.Concurrent
import qualified Control.Exception  as E

(|<<) = fmap
{-# INLINE (|<<) #-}

(>>|) = flip fmap
{-# INLINE (>>|) #-}

delay = threadDelay . round . (*) 1e6

catchAny x y = x `E.catch` (\(e :: E.SomeException) -> y e)
