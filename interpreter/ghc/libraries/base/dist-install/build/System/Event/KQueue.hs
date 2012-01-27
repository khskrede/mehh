{-# LINE 1 "libraries/base/./System/Event/KQueue.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface, GeneralizedNewtypeDeriving,
{-# LINE 2 "libraries/base/./System/Event/KQueue.hsc" #-}
    NoImplicitPrelude, RecordWildCards #-}

module System.Event.KQueue
    (
      new
    , available
    ) where

import qualified System.Event.Internal as E


{-# LINE 13 "libraries/base/./System/Event/KQueue.hsc" #-}

{-# LINE 14 "libraries/base/./System/Event/KQueue.hsc" #-}
import GHC.Base

new :: IO E.Backend
new = error "KQueue back end not implemented for this platform"

available :: Bool
available = False
{-# INLINE available #-}

{-# LINE 299 "libraries/base/./System/Event/KQueue.hsc" #-}
