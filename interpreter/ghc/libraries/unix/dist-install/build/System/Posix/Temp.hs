{-# LINE 1 "libraries/unix/./System/Posix/Temp.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LINE 2 "libraries/unix/./System/Posix/Temp.hsc" #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Temp
-- Copyright   :  (c) Volker Stolz <vs@foldr.org>
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  vs@foldr.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX environment support
--
-----------------------------------------------------------------------------

module System.Posix.Temp (

    mkstemp

{- Not ported (yet?):
    tmpfile: can we handle FILE*?
    tmpnam: ISO C, should go in base?
    tempname: dito
-}

) where


{-# LINE 29 "libraries/unix/./System/Posix/Temp.hsc" #-}

import System.IO
import System.Posix.IO
import System.Posix.Types
import Foreign.C

-- |'mkstemp' - make a unique filename and open it for
-- reading\/writing (only safe on GHC & Hugs).
-- The returned 'FilePath' is the (possibly relative) path of
-- the created file, which is padded with 6 random characters.
mkstemp :: String -> IO (FilePath, Handle)
mkstemp template = do

{-# LINE 42 "libraries/unix/./System/Posix/Temp.hsc" #-}
  withCString template $ \ ptr -> do
    fd <- throwErrnoIfMinus1 "mkstemp" (c_mkstemp ptr)
    name <- peekCString ptr
    h <- fdToHandle (Fd fd)
    return (name, h)

{-# LINE 64 "libraries/unix/./System/Posix/Temp.hsc" #-}

foreign import ccall unsafe "HsUnix.h __hscore_mkstemp"
  c_mkstemp :: CString -> IO CInt

