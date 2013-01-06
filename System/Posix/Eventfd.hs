-- module:        Control.Event.Eventfd
-- author:        Alexander V Vershilov
-- stability:     experimental
-- porability:    unportable (ghc, linux>=2.6.30, posix)
-- licence:       MIT
--
-- | this module provides functionality for handling 
-- eventfd subsytem to send and receive events
-- 
-- 'EventFD' is an object that can be used as an event
-- wait/notify mechanism by userspace applications, and by
-- the kernel to notify userspace applications of events.
--
-- The object contains an unsigned 64-bit integer counter
-- that is maintained by the kernel.
--
-- Aplications can use eventfd descriptor instead of
-- a pipe in all cases where pipe is used to simply
-- signal events.
--
{-# LANGUAGE ScopedTypeVariables #-}
module System.Posix.Eventfd (
    EventFd(..)
  , eventfd
  , efdCloexec
  , efdSemaphore
  , eventfdRead
  , eventfdWrite
  , eventfdClose
  ) where

import Control.Applicative

import Data.Bits
import Data.Word

import GHC.Conc

import System.Posix.Eventfd.FFI.Eventfd 
import System.Posix.Types
import System.Posix.IO.ByteString
import Foreign
import Foreign.C.Types
import Foreign.C.Error


-- | Fd wrapper
newtype EventFd = EventFd { unEventFd :: Fd }

--  | creates  an "eventfd object" that can be used as an event
--  wait/notify mechanism by user-space applications, and by the  kernel
--  to notify user-space applications of events.  The object contains an
--  unsigned 64-bit integer (uint64_t) counter that is maintained by the
--  kernel.  This counter is initialized with the value specified in the
--  argument initval.
--
--  The following values may be bitwise ORed in flags to change the  be-
--  haviour of eventfd():
--
--     * efdCloexec - read FD_CLOEXEC
--
--     * efdSemaphore - provide  semaphore-like semantics for reads from 
--        the new file descriptor.  See below.
--
--  N.B. eventfd is always non-blocking as it couples with haskell RTS.
eventfd :: CUInt              -- ^ initial value
        -> [EFDFlag]          -- ^ flags
        -> IO EventFd
eventfd w f = EventFd . fromIntegral <$> (throwErrnoIfMinus1 "eventfd" $! c_eventfd w (efdFlags (efdNonblock:f)))

-- | When the file descriptor is no longer required it  should  be
-- closed.   When  all file descriptors associated with the same
-- eventfd object have been closed, the resources for object are
-- freed by the kernel.
eventfdClose :: EventFd -> IO ()
eventfdClose = (closeFdWith closeFd) . unEventFd


-- | Each successful 'eventfdRead' returns an 8-byte integer.  
--
-- The semantics  of 'eventfdRead'  depend  on  whether  the  eventfd
-- counter  currently  has  a  nonzero  value  and  whether  the
-- @efdSemaphore@ flag was specified when  creating  the  eventfd
-- file descriptor:
--
--   *  If 'efdSemaphore' was not specified and the eventfd counter
--      has a nonzero value, then a 'eventfdRead' returns 8 bytes  containing
--      that  value,  and the counter`s value is reset to zero.
--
--   *  If 'efdSemaphore' was specified and the eventfd counter has
--      a nonzero value, then a 'eventfdRead' returns 8 bytes containing
--      the value 1, and the counter's value is decremented by 1.
--
-- throws IOError if eventfd is closed while waiting for read.
eventfdRead :: EventFd -> IO Word64
eventfdRead f = threadWaitRead f' >> alloca (\x -> fdReadBuf f' (castPtr x) 8 >> peek x)
  where f' = unEventFd f

-- | A 'eventfdWrite' call adds the 8-byte integer value
-- The maximum value that may be stored in the counter is the 
-- largest unsigned 64-bit value  minus  1
-- (i.e.,  0xfffffffffffffffe).  If the addition would cause the
-- counter's value to exceed the maximum, then  the 'eventfdWrite'
-- either blocks until a 'eventfdRead' is  performed or eventfd
-- handle is closed.
--
-- throws IOError if eventfd is closed while waiting for read.
eventfdWrite :: EventFd -> Word64 -> IO CSize 
eventfdWrite fd w = threadWaitWrite f' >> with w (\x -> fdWriteBuf f' (castPtr x) 8)
  where f' = unEventFd fd

-- Convert list of flags to CInt
efdFlags :: [EFDFlag] -> CInt
efdFlags fs = foldr (.|.) 0 $! map unEFDFlag fs

