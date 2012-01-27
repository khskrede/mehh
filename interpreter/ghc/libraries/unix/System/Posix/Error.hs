-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Error
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX error support
--
-----------------------------------------------------------------------------

module System.Posix.Error (
	throwErrnoPath,
	throwErrnoPathIf, 
	throwErrnoPathIf_,
        throwErrnoPathIfRetry,
	throwErrnoPathIfNull,
	throwErrnoPathIfNullRetry,
	throwErrnoPathIfMinus1,
	throwErrnoPathIfMinus1_,
        throwErrnoPathIfMinus1Retry
  ) where

import Foreign
import Foreign.C

throwErrnoPathIfMinus1Retry :: Num a => String -> FilePath -> IO a -> IO a
throwErrnoPathIfMinus1Retry loc path f =
  throwErrnoPathIfRetry (== -1) loc path f

throwErrnoPathIfNullRetry :: String -> FilePath -> IO (Ptr a) -> IO (Ptr a)
throwErrnoPathIfNullRetry loc path f =
  throwErrnoPathIfRetry (== nullPtr) loc path f

throwErrnoPathIfRetry :: (a -> Bool) -> String -> FilePath -> IO a -> IO a
throwErrnoPathIfRetry pr loc path f =
  do
    res <- f
    if pr res
      then do
        err <- getErrno
        if err == eINTR
          then throwErrnoPathIfRetry pr loc path f
          else throwErrnoPath loc path
      else return res

