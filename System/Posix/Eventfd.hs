-- module: Control.Event.Eventfd
-- author: Alexander V Vershilov
-- licence: MIT
--
-- this module provides functionality for handling 
-- eventfd subsytem to send and receive events
-- 
-- 'eventtfd' is an object that can be used as an event
-- wait/notify mchanism by userspace applications, and by
-- the kernel to notify userspace applications of events.
--
-- The object contains an unsigned 64-bit integer counter
-- that is maintained by the kernel.
--
-- Aplications can use eventfd descriptor instead of
-- a pipe in all cases where pipe is used to simply
-- signal events.
--
-- TODO: make naive API
--       create tests of different program types
--       API: create fd
--            make file fd
--            use file as fd
--            pool on fd
--      test blocking IO 
--      create nonblocking only
{-# LANGUAGE EmptyDataDecls #-}
module System.Posix.Eventfd (
  EventFD
  , eventfd
  , eventfdRead
  , eventfdWrite
  , eventfdSem
  , eventfdUp
  , eventfdUpMany
  , eventfdDown
  , efdCloExec
  , efdNonBlock
  ) where



import qualified System.Posix.Eventfd.FFI.Eventfd as F
import System.Posix.Types
import System.Posix.IO.ByteString
import Data.Bits
import Data.Word
import Data.Binary
import Foreign
import Foreign.C.Types


newtype EFDFlag = EFDFlag { unEFDFlag :: CInt }

efdSemaphore :: EFDFlag 
efdSemaphore = EFDFlag 1
efdCloExec   = EFDFlag 2000000
efdNonBlock  = EFDFlag 4000

-- EventFD data
data EventFD a = EventFD Fd

-- phantom types to describe data and semaphore eventfd
data Sem
data Dat

-- | Event fd creates an "eventfd object"
--   possible flags:
--    EFD_CLOEXEC read FD_CLOEXEC
--    EFD_NONBLOCK - set O_NONBLOCK file status fag on a
--      new open file description. 
eventfd :: CUInt -> CInt -> IO Fd
eventfd w f = fmap fromIntegral (F.eventfd w f)

-- | Open semaphore live eventfd
eventfdSem :: CUInt -> [EFDFlag] -> IO (EventFD Sem)
eventfdSem w fs = fmap EventFD (eventfd w (concatFlags (efdSemaphore:fs)))

-- | Open data eventfd
eventfdDat :: CUInt -> [EFDFlag] -> IO (EventFD Dat)
eventfdDat w fs = fmap EventFD (eventfd w (concatFlags fs))

-- | each successful read returns 'Word64'. 
-- Semantics:
--  * if EFD_SEMAPHORE is not specified and counter as a nonzero value
--  then read returns 8 bytes containing that value, and the counter's
--  value is reset to zero
--  * EFD_SEMAPHORE is set and eventfd has nonzero value -> return 1
--  and counter is decremented by 1
--  * EFD_SEMAPHORE is zero then call either blocks (TODO) until counter
--  becomes nonzero or fails with EAGAIN if it was nonblocking
eventfdRead :: Fd -> IO Word64
eventfdRead f = alloca $ \x -> fdReadBuf f (castPtr x) (fromIntegral 8) >> peek x

eventfdUp :: (EventFD Sem) -> IO ()
eventfdUp (EventFD fd) = eventfdWrite fd 1 >> return ()
 
eventfdUpMany :: (EventFD Sem) -> Int -> IO ()
eventfdUpMany (EventFD fd) n = eventfdWrite fd (fromIntegral n) >> return ()

-- | read eventfd if event fd has 
-- |  * nonzero value -> return 1 and counter decremented by 1
-- |  * zero value -> either blocks until a counter becomes nonzero or fails
eventfdDown :: (EventFD Sem) -> IO ()
eventfdDown (EventFD fd) = eventfdRead fd >> return ()

-- | write call adds 8-byte integer value to the counter. 
-- if addition would cause the counter's value to exceed maximum then 
-- write either blocks until a read is performed on file descriptor or
-- fails with the read EAGAIN if it was nonblocking
eventfdWrite :: Fd -> Word64 -> IO CSize 
eventfdWrite fd w = with w (\x -> fdWriteBuf fd (castPtr x) 8)

concatFlags :: [EFDFlag] -> CInt
concatFlags xs = foldr (.|.) 0 $ map unEFDFlag xs
