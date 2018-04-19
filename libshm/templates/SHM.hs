{-

  Haskell SHM Bindings

  Short and simple; syntactical improvement from C bindings so groups/variables can be passed around as values but otherwise identical in functionality.

  Notes:
  1. Calling shm_init is mandatory.
  2. Use "withGroup" to properly bracket potentially exception-throwing code such as to avoid deadlocks.
  3. ByteString is used instead of the native Haskell type (character linked list) for efficiency.

-}

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}

module SHM where

import qualified Data.Aeson            as A
import qualified Data.Binary           as B
import qualified Data.ByteString.Char8 as B
import qualified Data.Text             as T
import qualified Control.Exception     as E
import Foreign.C.String
import GHC.Generics

foreign import ccall "shm_init" shm_init :: IO ()

instance A.FromJSON (SHM.Var Double) where
<!--(for g in groups)-->
  <!--(for k in g['varnames'])--> 
    <!--(if g['vars'][k]['type'] == 'double')-->
    parseJSON (A.String "$!g['groupname']!$_$!k!$") = pure $!g['groupname']!$_$!k!$
    <!--(end)-->
  <!--(end)-->
<!--(end)--> 
    parseJSON _ = mempty

instance A.ToJSON (SHM.Var a) where
  toJSON var = A.String $ varName var

instance (A.FromJSON (SHM.Var a)) => B.Binary (SHM.Var a) where
  put v = let A.String s = A.toJSON v in B.put s
  get   = do
            val <- B.get
            case A.fromJSON (A.String val) of
              A.Success var -> return var
              _             -> fail "Parse failed!"

instance Eq (SHM.Var a) where
  (==) vx vy = varName vx == varName vy

instance Show (SHM.Var a) where
  show = T.unpack . varName

withGroup :: Group -> IO a -> IO a
withGroup x y = E.bracket_
  (groupLock x)
  (groupUnlock x)
  y

data Group = Group {
  groupLock :: IO (),
  groupUnlock :: IO ()
}

data Var a = Var {
  varGet   :: IO a,
  varSet   :: a -> IO (),
  varName  :: T.Text,
  varGroup :: Group
} deriving (Generic)

($<) :: forall a . Var a -> IO a
($<) = varGet
{-# INLINE ($<) #-}

($>) :: forall a . Var a -> a -> IO ()
($>) = varSet
{-# INLINE ($>) #-}

<!--(for g in groups)-->
foreign import ccall "shm_lock_$!g['groupname']!$" shm_$!g['groupname']!$_lock :: IO ()
foreign import ccall "shm_unlock_$!g['groupname']!$" shm_$!g['groupname']!$_unlock :: IO ()

$!g['groupname']!$ :: Group
$!g['groupname']!$ = Group {
  groupLock = shm_$!g['groupname']!$_lock,
  groupUnlock = shm_$!g['groupname']!$_unlock
}

  <!--(for k in g['varnames'])-->
foreign import ccall "shm_set_$!g['groupname']!$_$!k!$" shm_$!g['groupname']!$_$!k!$_set :: $!g['vars'][k]['haskelltype']!$ -> IO ()
foreign import ccall "shm_get_$!g['groupname']!$_$!k!$" shm_$!g['groupname']!$_$!k!$_get :: IO $!g['vars'][k]['haskelltype']!$
  
    <!--(if g['vars'][k]['type'] =='string')-->
$!g['groupname']!$_$!k!$ :: Var B.ByteString
    <!--(else)-->
$!g['groupname']!$_$!k!$ :: Var $!g['vars'][k]['haskelltype']!$  
    <!--(end)-->
$!g['groupname']!$_$!k!$ = Var {
    <!--(if g['vars'][k]['type'] =='string')-->
  varGet = B.packCString =<< shm_$!g['groupname']!$_$!k!$_get,
  varSet = flip B.useAsCString shm_$!g['groupname']!$_$!k!$_set,
    <!--(else)-->
  varGet = shm_$!g['groupname']!$_$!k!$_get,
  varSet = shm_$!g['groupname']!$_$!k!$_set,
    <!--(end)-->
  varName  = "$!g['groupname']!$_$!k!$",
  varGroup = $!g['groupname']!$
}

  <!--(end)-->

<!--(end)-->
