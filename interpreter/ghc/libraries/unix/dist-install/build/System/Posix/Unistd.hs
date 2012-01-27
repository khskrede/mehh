{-# LINE 1 "libraries/unix/./System/Posix/Unistd.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LINE 2 "libraries/unix/./System/Posix/Unistd.hsc" #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-binds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Unistd
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX miscellaneous stuff, mostly from unistd.h
--
-----------------------------------------------------------------------------

module System.Posix.Unistd (
    -- * System environment
    SystemID(..),
    getSystemID,

    SysVar(..),
    getSysVar,

    -- * Sleeping
    sleep, usleep, nanosleep,

  {-
    ToDo from unistd.h:
      confstr, 
      lots of sysconf variables

    -- use Network.BSD
    gethostid, gethostname

    -- should be in System.Posix.Files?
    pathconf, fpathconf,

    -- System.Posix.Signals
    ualarm,

    -- System.Posix.IO
    read, write,

    -- should be in System.Posix.User?
    getEffectiveUserName,
-}
  ) where


{-# LINE 51 "libraries/unix/./System/Posix/Unistd.hsc" #-}

import Foreign.C.Error
import Foreign.C.String ( peekCString )
import Foreign.C.Types
import Foreign
import System.Posix.Types
import System.Posix.Internals

-- -----------------------------------------------------------------------------
-- System environment (uname())

data SystemID =
  SystemID { systemName :: String
  	   , nodeName   :: String
	   , release    :: String
	   , version    :: String
	   , machine    :: String
	   }

getSystemID :: IO SystemID
getSystemID = do
  allocaBytes (390) $ \p_sid -> do
{-# LINE 73 "libraries/unix/./System/Posix/Unistd.hsc" #-}
    throwErrnoIfMinus1_ "getSystemID" (c_uname p_sid)
    sysN <- peekCString (((\hsc_ptr -> hsc_ptr `plusPtr` 0)) p_sid)
{-# LINE 75 "libraries/unix/./System/Posix/Unistd.hsc" #-}
    node <- peekCString (((\hsc_ptr -> hsc_ptr `plusPtr` 65)) p_sid)
{-# LINE 76 "libraries/unix/./System/Posix/Unistd.hsc" #-}
    rel  <- peekCString (((\hsc_ptr -> hsc_ptr `plusPtr` 130)) p_sid)
{-# LINE 77 "libraries/unix/./System/Posix/Unistd.hsc" #-}
    ver  <- peekCString (((\hsc_ptr -> hsc_ptr `plusPtr` 195)) p_sid)
{-# LINE 78 "libraries/unix/./System/Posix/Unistd.hsc" #-}
    mach <- peekCString (((\hsc_ptr -> hsc_ptr `plusPtr` 260)) p_sid)
{-# LINE 79 "libraries/unix/./System/Posix/Unistd.hsc" #-}
    return (SystemID { systemName = sysN,
		       nodeName   = node,
		       release    = rel,
		       version    = ver,
		       machine    = mach
		     })

foreign import ccall unsafe "uname"
   c_uname :: Ptr CUtsname -> IO CInt

-- -----------------------------------------------------------------------------
-- sleeping

-- | Sleep for the specified duration (in seconds).  Returns the time remaining
-- (if the sleep was interrupted by a signal, for example).
--
-- GHC Note: the comment for 'usleep' also applies here.
--
sleep :: Int -> IO Int
sleep 0 = return 0
sleep secs = do r <- c_sleep (fromIntegral secs); return (fromIntegral r)

foreign import ccall safe "sleep"
  c_sleep :: CUInt -> IO CUInt

-- | Sleep for the specified duration (in microseconds).
--
-- GHC Note: 'Control.Concurrent.threadDelay' is a better choice.
-- Without the @-threaded@ option, 'usleep' will block all other user
-- threads.  Even with the @-threaded@ option, 'usleep' requires a
-- full OS thread to itself.  'Control.Concurrent.threadDelay' has
-- neither of these shortcomings.
--
usleep :: Int -> IO ()

{-# LINE 114 "libraries/unix/./System/Posix/Unistd.hsc" #-}
usleep usecs = nanosleep (fromIntegral usecs * 1000)

{-# LINE 131 "libraries/unix/./System/Posix/Unistd.hsc" #-}

-- | Sleep for the specified duration (in nanoseconds)
--
nanosleep :: Integer -> IO ()

{-# LINE 138 "libraries/unix/./System/Posix/Unistd.hsc" #-}
nanosleep 0 = return ()
nanosleep nsecs = do
  allocaBytes (16) $ \pts1 -> do
{-# LINE 141 "libraries/unix/./System/Posix/Unistd.hsc" #-}
  allocaBytes (16) $ \pts2 -> do
{-# LINE 142 "libraries/unix/./System/Posix/Unistd.hsc" #-}
     let (tv_sec0, tv_nsec0) = nsecs `divMod` 1000000000
     let 
       loop tv_sec tv_nsec = do
         ((\hsc_ptr -> pokeByteOff hsc_ptr 0))  pts1 tv_sec
{-# LINE 146 "libraries/unix/./System/Posix/Unistd.hsc" #-}
         ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) pts1 tv_nsec
{-# LINE 147 "libraries/unix/./System/Posix/Unistd.hsc" #-}
         res <- c_nanosleep pts1 pts2
         if res == 0
            then return ()
            else do errno <- getErrno
                    if errno == eINTR
                       then do
                           tv_sec'  <- ((\hsc_ptr -> peekByteOff hsc_ptr 0))  pts2
{-# LINE 154 "libraries/unix/./System/Posix/Unistd.hsc" #-}
                           tv_nsec' <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) pts2
{-# LINE 155 "libraries/unix/./System/Posix/Unistd.hsc" #-}
                           loop tv_sec' tv_nsec'
                       else throwErrno "nanosleep"
     loop (fromIntegral tv_sec0 :: CTime) (fromIntegral tv_nsec0 :: CTime)

data CTimeSpec

foreign import ccall safe "__hsunix_nanosleep" 
  c_nanosleep :: Ptr CTimeSpec -> Ptr CTimeSpec -> IO CInt

{-# LINE 164 "libraries/unix/./System/Posix/Unistd.hsc" #-}

-- -----------------------------------------------------------------------------
-- System variables

data SysVar = ArgumentLimit
            | ChildLimit
            | ClockTick
            | GroupLimit
            | OpenFileLimit
            | PosixVersion
            | HasSavedIDs
            | HasJobControl
	-- ToDo: lots more

getSysVar :: SysVar -> IO Integer
getSysVar v =
    case v of
      ArgumentLimit -> sysconf (0)
{-# LINE 182 "libraries/unix/./System/Posix/Unistd.hsc" #-}
      ChildLimit    -> sysconf (1)
{-# LINE 183 "libraries/unix/./System/Posix/Unistd.hsc" #-}
      ClockTick	    -> sysconf (2)
{-# LINE 184 "libraries/unix/./System/Posix/Unistd.hsc" #-}
      GroupLimit    -> sysconf (3)
{-# LINE 185 "libraries/unix/./System/Posix/Unistd.hsc" #-}
      OpenFileLimit -> sysconf (4)
{-# LINE 186 "libraries/unix/./System/Posix/Unistd.hsc" #-}
      PosixVersion  -> sysconf (29)
{-# LINE 187 "libraries/unix/./System/Posix/Unistd.hsc" #-}
      HasSavedIDs   -> sysconf (8)
{-# LINE 188 "libraries/unix/./System/Posix/Unistd.hsc" #-}
      HasJobControl -> sysconf (7)
{-# LINE 189 "libraries/unix/./System/Posix/Unistd.hsc" #-}

sysconf :: CInt -> IO Integer
sysconf n = do 
  r <- throwErrnoIfMinus1 "getSysVar" (c_sysconf n)
  return (fromIntegral r)

foreign import ccall unsafe "sysconf"
  c_sysconf :: CInt -> IO CLong
