{-# LANGUAGE FlexibleInstances, DeriveDataTypeable #-}

module FFIAux where

import Foreign.C
import Foreign.Ptr
import Data.Typeable

data Wrapped = WPtr (Ptr ()) | WInt CInt | WDouble CDouble deriving (Typeable, Show)

class Wrappable a where
  wrap :: a -> Wrapped
  unwrap :: Wrapped -> a

instance Wrappable (Ptr ()) where 
  wrap = WPtr
  unwrap (WPtr x) = x

instance Wrappable CInt where
  wrap = WInt
  unwrap (WInt x) = x

instance Wrappable CDouble where
  wrap = WDouble
  unwrap (WDouble x) = x
