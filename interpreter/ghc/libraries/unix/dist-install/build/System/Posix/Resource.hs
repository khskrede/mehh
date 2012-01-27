{-# LINE 1 "libraries/unix/./System/Posix/Resource.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LINE 2 "libraries/unix/./System/Posix/Resource.hsc" #-}
{-# OPTIONS_GHC -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/WorkingConventions#Warnings
-- for details
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Resource
-- Copyright   :  (c) The University of Glasgow 2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX resource support
--
-----------------------------------------------------------------------------

module System.Posix.Resource (
    -- * Resource Limits
    ResourceLimit(..), ResourceLimits(..), Resource(..),
    getResourceLimit,
    setResourceLimit,
  ) where


{-# LINE 30 "libraries/unix/./System/Posix/Resource.hsc" #-}

import System.Posix.Types
import Foreign
import Foreign.C

-- -----------------------------------------------------------------------------
-- Resource limits

data Resource
  = ResourceCoreFileSize
  | ResourceCPUTime
  | ResourceDataSize
  | ResourceFileSize
  | ResourceOpenFiles
  | ResourceStackSize

{-# LINE 46 "libraries/unix/./System/Posix/Resource.hsc" #-}
  | ResourceTotalMemory

{-# LINE 48 "libraries/unix/./System/Posix/Resource.hsc" #-}
  deriving Eq

data ResourceLimits
  = ResourceLimits { softLimit, hardLimit :: ResourceLimit }
  deriving Eq

data ResourceLimit
  = ResourceLimitInfinity
  | ResourceLimitUnknown
  | ResourceLimit Integer
  deriving Eq

type RLimit = ()

foreign import ccall unsafe "HsUnix.h __hscore_getrlimit"
  c_getrlimit :: CInt -> Ptr RLimit -> IO CInt

foreign import ccall unsafe "HsUnix.h __hscore_setrlimit"
  c_setrlimit :: CInt -> Ptr RLimit -> IO CInt

getResourceLimit :: Resource -> IO ResourceLimits
getResourceLimit res = do
  allocaBytes (16) $ \p_rlimit -> do
{-# LINE 71 "libraries/unix/./System/Posix/Resource.hsc" #-}
    throwErrnoIfMinus1 "getResourceLimit" $
      c_getrlimit (packResource res) p_rlimit
    soft <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) p_rlimit
{-# LINE 74 "libraries/unix/./System/Posix/Resource.hsc" #-}
    hard <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) p_rlimit
{-# LINE 75 "libraries/unix/./System/Posix/Resource.hsc" #-}
    return (ResourceLimits { 
		softLimit = unpackRLimit soft,
		hardLimit = unpackRLimit hard
	   })

setResourceLimit :: Resource -> ResourceLimits -> IO ()
setResourceLimit res ResourceLimits{softLimit=soft,hardLimit=hard} = do
  allocaBytes (16) $ \p_rlimit -> do
{-# LINE 83 "libraries/unix/./System/Posix/Resource.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) p_rlimit (packRLimit soft True)
{-# LINE 84 "libraries/unix/./System/Posix/Resource.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) p_rlimit (packRLimit hard False)
{-# LINE 85 "libraries/unix/./System/Posix/Resource.hsc" #-}
    throwErrnoIfMinus1 "setResourceLimit" $
	c_setrlimit (packResource res) p_rlimit
    return ()

packResource :: Resource -> CInt
packResource ResourceCoreFileSize  = (4)
{-# LINE 91 "libraries/unix/./System/Posix/Resource.hsc" #-}
packResource ResourceCPUTime       = (0)
{-# LINE 92 "libraries/unix/./System/Posix/Resource.hsc" #-}
packResource ResourceDataSize      = (2)
{-# LINE 93 "libraries/unix/./System/Posix/Resource.hsc" #-}
packResource ResourceFileSize      = (1)
{-# LINE 94 "libraries/unix/./System/Posix/Resource.hsc" #-}
packResource ResourceOpenFiles     = (7)
{-# LINE 95 "libraries/unix/./System/Posix/Resource.hsc" #-}
packResource ResourceStackSize     = (3)
{-# LINE 96 "libraries/unix/./System/Posix/Resource.hsc" #-}

{-# LINE 97 "libraries/unix/./System/Posix/Resource.hsc" #-}
packResource ResourceTotalMemory   = (9)
{-# LINE 98 "libraries/unix/./System/Posix/Resource.hsc" #-}

{-# LINE 99 "libraries/unix/./System/Posix/Resource.hsc" #-}

unpackRLimit :: CRLim -> ResourceLimit
unpackRLimit (18446744073709551615)  = ResourceLimitInfinity
{-# LINE 102 "libraries/unix/./System/Posix/Resource.hsc" #-}

{-# LINE 103 "libraries/unix/./System/Posix/Resource.hsc" #-}
unpackRLimit (18446744073709551615) = ResourceLimitUnknown
{-# LINE 104 "libraries/unix/./System/Posix/Resource.hsc" #-}
unpackRLimit (18446744073709551615) = ResourceLimitUnknown
{-# LINE 105 "libraries/unix/./System/Posix/Resource.hsc" #-}

{-# LINE 106 "libraries/unix/./System/Posix/Resource.hsc" #-}
unpackRLimit other = ResourceLimit (fromIntegral other)

packRLimit :: ResourceLimit -> Bool -> CRLim
packRLimit ResourceLimitInfinity _     = (18446744073709551615)
{-# LINE 110 "libraries/unix/./System/Posix/Resource.hsc" #-}

{-# LINE 111 "libraries/unix/./System/Posix/Resource.hsc" #-}
packRLimit ResourceLimitUnknown  True  = (18446744073709551615)
{-# LINE 112 "libraries/unix/./System/Posix/Resource.hsc" #-}
packRLimit ResourceLimitUnknown  False = (18446744073709551615)
{-# LINE 113 "libraries/unix/./System/Posix/Resource.hsc" #-}

{-# LINE 114 "libraries/unix/./System/Posix/Resource.hsc" #-}
packRLimit (ResourceLimit other) _     = fromIntegral other


-- -----------------------------------------------------------------------------
-- Test code

{-
import System.Posix
import Control.Monad

main = do
 zipWithM_ (\r n -> setResourceLimit r ResourceLimits{
					hardLimit = ResourceLimit n,
					softLimit = ResourceLimit n })
	allResources [1..]	
 showAll
 mapM_ (\r -> setResourceLimit r ResourceLimits{
					hardLimit = ResourceLimit 1,
					softLimit = ResourceLimitInfinity })
	allResources
   -- should fail


showAll = 
  mapM_ (\r -> getResourceLimit r >>= (putStrLn . showRLims)) allResources

allResources =
    [ResourceCoreFileSize, ResourceCPUTime, ResourceDataSize,
	ResourceFileSize, ResourceOpenFiles, ResourceStackSize
#ifdef RLIMIT_AS
	, ResourceTotalMemory 
#endif
	]

showRLims ResourceLimits{hardLimit=h,softLimit=s}
  = "hard: " ++ showRLim h ++ ", soft: " ++ showRLim s
 
showRLim ResourceLimitInfinity = "infinity"
showRLim ResourceLimitUnknown  = "unknown"
showRLim (ResourceLimit other)  = show other
-}
