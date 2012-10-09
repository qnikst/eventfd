{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module System.Posix.Eventfd.FFI.Eventfd where

-- TODO make safe functions

import Data.Int
import Foreign.C.Types
import Foreign

-- newtype EventFDFlag = EventFDFlag { unEventFDFlag -> CInt }
foreign import ccall "sys/eventfd.h eventfd" eventfd 
  :: CUInt -> CInt -> IO CInt
