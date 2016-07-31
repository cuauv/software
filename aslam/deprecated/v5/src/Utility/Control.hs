module Utility.Control where

import           Control.Concurrent
import qualified Control.Exception     as E
import           Data.Time.Clock.POSIX

(|<<) ∷ Functor f ⇒ (a → b) → f a → f b
(|<<) = fmap
{-# INLINE (|<<) #-}

(>>|) ∷ Functor f ⇒ f a → (a → b) → f b
(>>|) = flip fmap
{-# INLINE (>>|) #-}

delay ∷ Double → IO ()
delay = threadDelay . round . (*) 1e6
{-# INLINE delay #-}

catchAny ∷ IO a → (E.SomeException → IO a) → IO a
catchAny x y = x `E.catch` (\(e :: E.SomeException) -> y e)
{-# INLINE catchAny #-}

unixTime ∷ IO Double
unixTime = (fromRational . toRational) |<< getPOSIXTime
{-# INLINE unixTime #-}
