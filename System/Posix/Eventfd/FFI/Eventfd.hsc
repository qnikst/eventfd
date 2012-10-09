{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module System.Posix.Eventfd.FFI.Eventfd where

-- TODO add glibc function support
-- TODO make safe functions

import Data.Int
import Foreign.C.Types
import Foreign

#include <sys/eventfd.h>

newtype EFDFlag = EFDFlag { unEFDFlag :: CInt }

#{enum EFDFlag, EFDFlag
  , efdSemaphore = EFD_SEMAPHORE
  , efdCloexec   = EFD_CLOEXEC
  , efdNonblock  = EFD_NONBLOCK
}

foreign import ccall "sys/eventfd.h eventfd" c_eventfd 
  :: CUInt -> CInt -> IO CInt
