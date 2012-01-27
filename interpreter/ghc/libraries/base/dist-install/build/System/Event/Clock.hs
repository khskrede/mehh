{-# LINE 1 "libraries/base/./System/Event/Clock.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LINE 2 "libraries/base/./System/Event/Clock.hsc" #-}

module System.Event.Clock (getCurrentTime) where


{-# LINE 6 "libraries/base/./System/Event/Clock.hsc" #-}

import Foreign (Ptr, Storable(..), nullPtr, with)
import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.Types (CInt, CLong)
import GHC.Base
import GHC.Err
import GHC.Num
import GHC.Real

-- TODO: Implement this for Windows.

-- | Return the current time, in seconds since Jan. 1, 1970.
getCurrentTime :: IO Double
getCurrentTime = do
    tv <- with (CTimeval 0 0) $ \tvptr -> do
        throwErrnoIfMinus1_ "gettimeofday" (gettimeofday tvptr nullPtr)
        peek tvptr
    let !t = fromIntegral (sec tv) + fromIntegral (usec tv) / 1000000.0
    return t

------------------------------------------------------------------------
-- FFI binding

data CTimeval = CTimeval
    { sec  :: {-# UNPACK #-} !CLong
    , usec :: {-# UNPACK #-} !CLong
    }

instance Storable CTimeval where
    sizeOf _ = (16)
{-# LINE 36 "libraries/base/./System/Event/Clock.hsc" #-}
    alignment _ = alignment (undefined :: CLong)

    peek ptr = do
        sec' <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 40 "libraries/base/./System/Event/Clock.hsc" #-}
        usec' <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 41 "libraries/base/./System/Event/Clock.hsc" #-}
        return $ CTimeval sec' usec'

    poke ptr tv = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr (sec tv)
{-# LINE 45 "libraries/base/./System/Event/Clock.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr (usec tv)
{-# LINE 46 "libraries/base/./System/Event/Clock.hsc" #-}

foreign import ccall unsafe "sys/time.h gettimeofday" gettimeofday
    :: Ptr CTimeval -> Ptr () -> IO CInt
