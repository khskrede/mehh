{-# LINE 1 "libraries/base/./System/CPUTime.hsc" #-}
-----------------------------------------------------------------------------
{-# LINE 2 "libraries/base/./System/CPUTime.hsc" #-}
-- |
-- Module      :  System.CPUTime
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The standard CPUTime library.
--
-----------------------------------------------------------------------------

module System.CPUTime 
        (
         getCPUTime,       -- :: IO Integer
         cpuTimePrecision  -- :: Integer
        ) where

import Prelude

import Data.Ratio


{-# LINE 28 "libraries/base/./System/CPUTime.hsc" #-}


{-# LINE 32 "libraries/base/./System/CPUTime.hsc" #-}


{-# LINE 34 "libraries/base/./System/CPUTime.hsc" #-}
import Foreign hiding (unsafePerformIO)
import Foreign.C

{-# LINE 37 "libraries/base/./System/CPUTime.hsc" #-}
import System.IO.Unsafe (unsafePerformIO)

{-# LINE 39 "libraries/base/./System/CPUTime.hsc" #-}


{-# LINE 41 "libraries/base/./System/CPUTime.hsc" #-}

-- For _SC_CLK_TCK

{-# LINE 44 "libraries/base/./System/CPUTime.hsc" #-}

{-# LINE 45 "libraries/base/./System/CPUTime.hsc" #-}

{-# LINE 46 "libraries/base/./System/CPUTime.hsc" #-}

-- For struct rusage

{-# LINE 49 "libraries/base/./System/CPUTime.hsc" #-}

{-# LINE 50 "libraries/base/./System/CPUTime.hsc" #-}

{-# LINE 51 "libraries/base/./System/CPUTime.hsc" #-}

{-# LINE 52 "libraries/base/./System/CPUTime.hsc" #-}

{-# LINE 53 "libraries/base/./System/CPUTime.hsc" #-}

-- For FILETIME etc. on Windows

{-# LINE 58 "libraries/base/./System/CPUTime.hsc" #-}

-- for CLK_TCK

{-# LINE 61 "libraries/base/./System/CPUTime.hsc" #-}

{-# LINE 62 "libraries/base/./System/CPUTime.hsc" #-}

{-# LINE 63 "libraries/base/./System/CPUTime.hsc" #-}

-- for struct tms

{-# LINE 66 "libraries/base/./System/CPUTime.hsc" #-}

{-# LINE 67 "libraries/base/./System/CPUTime.hsc" #-}

{-# LINE 68 "libraries/base/./System/CPUTime.hsc" #-}


{-# LINE 70 "libraries/base/./System/CPUTime.hsc" #-}


{-# LINE 72 "libraries/base/./System/CPUTime.hsc" #-}
realToInteger :: Real a => a -> Integer
realToInteger ct = round (realToFrac ct :: Double)
  -- CTime, CClock, CUShort etc are in Real but not Fractional, 
  -- so we must convert to Double before we can round it

{-# LINE 77 "libraries/base/./System/CPUTime.hsc" #-}


{-# LINE 79 "libraries/base/./System/CPUTime.hsc" #-}
-- -----------------------------------------------------------------------------
-- |Computation 'getCPUTime' returns the number of picoseconds CPU time
-- used by the current program.  The precision of this result is
-- implementation-dependent.

getCPUTime :: IO Integer
getCPUTime = do


{-# LINE 88 "libraries/base/./System/CPUTime.hsc" #-}
-- getrusage() is right royal pain to deal with when targetting multiple
-- versions of Solaris, since some versions supply it in libc (2.3 and 2.5),
-- while 2.4 has got it in libucb (I wouldn't be too surprised if it was back
-- again in libucb in 2.6..)
--
-- Avoid the problem by resorting to times() instead.
--

{-# LINE 96 "libraries/base/./System/CPUTime.hsc" #-}
    allocaBytes (144) $ \ p_rusage -> do
{-# LINE 97 "libraries/base/./System/CPUTime.hsc" #-}
    throwErrnoIfMinus1_ "getrusage" $ getrusage (0) p_rusage
{-# LINE 98 "libraries/base/./System/CPUTime.hsc" #-}

    let ru_utime = ((\hsc_ptr -> hsc_ptr `plusPtr` 0)) p_rusage
{-# LINE 100 "libraries/base/./System/CPUTime.hsc" #-}
    let ru_stime = ((\hsc_ptr -> hsc_ptr `plusPtr` 16)) p_rusage
{-# LINE 101 "libraries/base/./System/CPUTime.hsc" #-}
    u_sec  <- ((\hsc_ptr -> peekByteOff hsc_ptr 0))  ru_utime :: IO CTime
{-# LINE 102 "libraries/base/./System/CPUTime.hsc" #-}
    u_usec <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ru_utime :: IO CTime
{-# LINE 103 "libraries/base/./System/CPUTime.hsc" #-}
    s_sec  <- ((\hsc_ptr -> peekByteOff hsc_ptr 0))  ru_stime :: IO CTime
{-# LINE 104 "libraries/base/./System/CPUTime.hsc" #-}
    s_usec <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ru_stime :: IO CTime
{-# LINE 105 "libraries/base/./System/CPUTime.hsc" #-}
    return ((realToInteger u_sec * 1000000 + realToInteger u_usec + 
             realToInteger s_sec * 1000000 + realToInteger s_usec) 
                * 1000000)

type CRUsage = ()
foreign import ccall unsafe getrusage :: CInt -> Ptr CRUsage -> IO CInt

{-# LINE 129 "libraries/base/./System/CPUTime.hsc" #-}


{-# LINE 163 "libraries/base/./System/CPUTime.hsc" #-}

{-# LINE 164 "libraries/base/./System/CPUTime.hsc" #-}

-- |The 'cpuTimePrecision' constant is the smallest measurable difference
-- in CPU time that the implementation can record, and is given as an
-- integral number of picoseconds.


{-# LINE 170 "libraries/base/./System/CPUTime.hsc" #-}
cpuTimePrecision :: Integer
cpuTimePrecision = round ((1000000000000::Integer) % fromIntegral (clockTicks))

{-# LINE 173 "libraries/base/./System/CPUTime.hsc" #-}


{-# LINE 175 "libraries/base/./System/CPUTime.hsc" #-}
clockTicks :: Int
clockTicks =

{-# LINE 180 "libraries/base/./System/CPUTime.hsc" #-}
    unsafePerformIO (sysconf (2) >>= return . fromIntegral)
{-# LINE 181 "libraries/base/./System/CPUTime.hsc" #-}
foreign import ccall unsafe sysconf :: CInt -> IO CLong

{-# LINE 183 "libraries/base/./System/CPUTime.hsc" #-}

{-# LINE 184 "libraries/base/./System/CPUTime.hsc" #-}
