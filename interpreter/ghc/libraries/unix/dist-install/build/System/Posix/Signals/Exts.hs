{-# LINE 1 "libraries/unix/./System/Posix/Signals/Exts.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LINE 2 "libraries/unix/./System/Posix/Signals/Exts.hsc" #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Signals.Exts
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX, includes Linuxisms/BSDisms)
--
-- non-POSIX signal support commonly available
--
-----------------------------------------------------------------------------


{-# LINE 17 "libraries/unix/./System/Posix/Signals/Exts.hsc" #-}

module System.Posix.Signals.Exts (
  module System.Posix.Signals


{-# LINE 24 "libraries/unix/./System/Posix/Signals/Exts.hsc" #-}

{-# LINE 25 "libraries/unix/./System/Posix/Signals/Exts.hsc" #-}
  , windowChange, sigWINCH

{-# LINE 27 "libraries/unix/./System/Posix/Signals/Exts.hsc" #-}

  ) where

import Foreign.C ( CInt )
import System.Posix.Signals


{-# LINE 41 "libraries/unix/./System/Posix/Signals/Exts.hsc" #-}

{-# LINE 44 "libraries/unix/./System/Posix/Signals/Exts.hsc" #-}

{-# LINE 45 "libraries/unix/./System/Posix/Signals/Exts.hsc" #-}
foreign import ccall unsafe "__hsunix_SIGWINCH"   sigWINCH   :: CInt

{-# LINE 47 "libraries/unix/./System/Posix/Signals/Exts.hsc" #-}

{-# LINE 48 "libraries/unix/./System/Posix/Signals/Exts.hsc" #-}


{-# LINE 53 "libraries/unix/./System/Posix/Signals/Exts.hsc" #-}


{-# LINE 55 "libraries/unix/./System/Posix/Signals/Exts.hsc" #-}
windowChange :: Signal
windowChange = sigWINCH

{-# LINE 58 "libraries/unix/./System/Posix/Signals/Exts.hsc" #-}
