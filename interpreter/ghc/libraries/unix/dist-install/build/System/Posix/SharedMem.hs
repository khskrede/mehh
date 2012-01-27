{-# LINE 1 "libraries/unix/./System/Posix/SharedMem.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LINE 2 "libraries/unix/./System/Posix/SharedMem.hsc" #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.SharedMem
-- Copyright   :  (c) Daniel Franke 2007
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires POSIX)
--
-- POSIX shared memory support.
--
-----------------------------------------------------------------------------

module System.Posix.SharedMem
    (ShmOpenFlags(..), shmOpen, shmUnlink)
    where


{-# LINE 23 "libraries/unix/./System/Posix/SharedMem.hsc" #-}

{-# LINE 24 "libraries/unix/./System/Posix/SharedMem.hsc" #-}

{-# LINE 25 "libraries/unix/./System/Posix/SharedMem.hsc" #-}


{-# LINE 27 "libraries/unix/./System/Posix/SharedMem.hsc" #-}

import System.Posix.Types
import System.Posix.Error
import Foreign.C
import Data.Bits

data ShmOpenFlags = ShmOpenFlags 
    { shmReadWrite :: Bool,
      -- ^ If true, open the shm object read-write rather than read-only. 
      shmCreate :: Bool,
      -- ^ If true, create the shm object if it does not exist. 
      shmExclusive :: Bool,
      -- ^ If true, throw an exception if the shm object already exists.
      shmTrunc :: Bool
      -- ^ If true, wipe the contents of the shm object after opening it.
    }

-- | Open a shared memory object with the given name, flags, and mode.
shmOpen :: String -> ShmOpenFlags -> FileMode -> IO Fd

{-# LINE 47 "libraries/unix/./System/Posix/SharedMem.hsc" #-}
shmOpen name flags mode =
    do cflags0 <- return 0
       cflags1 <- return $ cflags0 .|. (if shmReadWrite flags
                                        then 2
{-# LINE 51 "libraries/unix/./System/Posix/SharedMem.hsc" #-}
                                        else 0)
{-# LINE 52 "libraries/unix/./System/Posix/SharedMem.hsc" #-}
       cflags2 <- return $ cflags1 .|. (if shmCreate flags then 64 
{-# LINE 53 "libraries/unix/./System/Posix/SharedMem.hsc" #-}
                                        else 0)
       cflags3 <- return $ cflags2 .|. (if shmExclusive flags 
                                        then 128 
{-# LINE 56 "libraries/unix/./System/Posix/SharedMem.hsc" #-}
                                        else 0)
       cflags4 <- return $ cflags3 .|. (if shmTrunc flags then 512 
{-# LINE 58 "libraries/unix/./System/Posix/SharedMem.hsc" #-}
                                        else 0)
       withCAString name (shmOpen' cflags4)
    where shmOpen' cflags cname =
              do fd <- throwErrnoIfMinus1 "shmOpen" $ 
                       shm_open cname cflags mode
                 return $ Fd fd

{-# LINE 67 "libraries/unix/./System/Posix/SharedMem.hsc" #-}

-- | Delete the shared memory object with the given name.
shmUnlink :: String -> IO ()

{-# LINE 71 "libraries/unix/./System/Posix/SharedMem.hsc" #-}
shmUnlink name = withCAString name shmUnlink'
    where shmUnlink' cname =
              throwErrnoIfMinus1_ "shmUnlink" $ shm_unlink cname

{-# LINE 77 "libraries/unix/./System/Posix/SharedMem.hsc" #-}


{-# LINE 79 "libraries/unix/./System/Posix/SharedMem.hsc" #-}
foreign import ccall unsafe "shm_open"
        shm_open :: CString -> CInt -> CMode -> IO CInt

{-# LINE 82 "libraries/unix/./System/Posix/SharedMem.hsc" #-}


{-# LINE 84 "libraries/unix/./System/Posix/SharedMem.hsc" #-}
foreign import ccall unsafe "shm_unlink"
        shm_unlink :: CString -> IO CInt

{-# LINE 87 "libraries/unix/./System/Posix/SharedMem.hsc" #-}
