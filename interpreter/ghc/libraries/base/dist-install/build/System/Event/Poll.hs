{-# LINE 1 "libraries/base/./System/Event/Poll.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface, GeneralizedNewtypeDeriving,
{-# LINE 2 "libraries/base/./System/Event/Poll.hsc" #-}
    NoImplicitPrelude #-}

module System.Event.Poll
    (
      new
    , available
    ) where


{-# LINE 11 "libraries/base/./System/Event/Poll.hsc" #-}


{-# LINE 22 "libraries/base/./System/Event/Poll.hsc" #-}

{-# LINE 23 "libraries/base/./System/Event/Poll.hsc" #-}

import Control.Concurrent.MVar (MVar, newMVar, swapMVar)
import Control.Monad ((=<<), liftM, liftM2, unless)
import Data.Bits (Bits, (.|.), (.&.))
import Data.Maybe (Maybe(..))
import Data.Monoid (Monoid(..))
import Foreign.C.Types (CInt, CShort, CULong)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))
import GHC.Base
import GHC.Conc.Sync (withMVar)
import GHC.Err (undefined)
import GHC.Num (Num(..))
import GHC.Real (ceiling, fromIntegral)
import GHC.Show (Show)
import System.Posix.Types (Fd(..))

import qualified System.Event.Array as A
import qualified System.Event.Internal as E

available :: Bool
available = True
{-# INLINE available #-}

data Poll = Poll {
      pollChanges :: {-# UNPACK #-} !(MVar (A.Array PollFd))
    , pollFd      :: {-# UNPACK #-} !(A.Array PollFd)
    }

new :: IO E.Backend
new = E.backend poll modifyFd (\_ -> return ()) `liftM`
      liftM2 Poll (newMVar =<< A.empty) A.empty

modifyFd :: Poll -> Fd -> E.Event -> E.Event -> IO ()
modifyFd p fd oevt nevt =
  withMVar (pollChanges p) $ \ary ->
    A.snoc ary $ PollFd fd (fromEvent nevt) (fromEvent oevt)

reworkFd :: Poll -> PollFd -> IO ()
reworkFd p (PollFd fd npevt opevt) = do
  let ary = pollFd p
  if opevt == 0
    then A.snoc ary $ PollFd fd npevt 0
    else do
      found <- A.findIndex ((== fd) . pfdFd) ary
      case found of
        Nothing        -> error "reworkFd: event not found"
        Just (i,_)
          | npevt /= 0 -> A.unsafeWrite ary i $ PollFd fd npevt 0
          | otherwise  -> A.removeAt ary i

poll :: Poll
     -> E.Timeout
     -> (Fd -> E.Event -> IO ())
     -> IO ()
poll p tout f = do
  let a = pollFd p
  mods <- swapMVar (pollChanges p) =<< A.empty
  A.forM_ mods (reworkFd p)
  n <- A.useAsPtr a $ \ptr len -> E.throwErrnoIfMinus1NoRetry "c_poll" $
         c_poll ptr (fromIntegral len) (fromIntegral (fromTimeout tout))
  unless (n == 0) $ do
    A.loop a 0 $ \i e -> do
      let r = pfdRevents e
      if r /= 0
        then do f (pfdFd e) (toEvent r)
                let i' = i + 1
                return (i', i' == n)
        else return (i, True)

fromTimeout :: E.Timeout -> Int
fromTimeout E.Forever     = -1
fromTimeout (E.Timeout s) = ceiling $ 1000 * s

data PollFd = PollFd {
      pfdFd      :: {-# UNPACK #-} !Fd
    , pfdEvents  :: {-# UNPACK #-} !Event
    , pfdRevents :: {-# UNPACK #-} !Event
    } deriving (Show)

newtype Event = Event CShort
    deriving (Eq, Show, Num, Storable, Bits)

pollIn     :: Event
pollIn     = Event 1
pollOut    :: Event
pollOut    = Event 4
pollErr    :: Event
pollErr    = Event 8
pollHup    :: Event
pollHup    = Event 16

{-# LINE 115 "libraries/base/./System/Event/Poll.hsc" #-}

fromEvent :: E.Event -> Event
fromEvent e = remap E.evtRead  pollIn .|.
              remap E.evtWrite pollOut
  where remap evt to
            | e `E.eventIs` evt = to
            | otherwise         = 0

toEvent :: Event -> E.Event
toEvent e = remap (pollIn .|. pollErr .|. pollHup)  E.evtRead `mappend`
            remap (pollOut .|. pollErr .|. pollHup) E.evtWrite
  where remap evt to
            | e .&. evt /= 0 = to
            | otherwise      = mempty

instance Storable PollFd where
    sizeOf _    = (8)
{-# LINE 132 "libraries/base/./System/Event/Poll.hsc" #-}
    alignment _ = alignment (undefined :: CInt)

    peek ptr = do
      fd <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 136 "libraries/base/./System/Event/Poll.hsc" #-}
      events <- (\hsc_ptr -> peekByteOff hsc_ptr 4) ptr
{-# LINE 137 "libraries/base/./System/Event/Poll.hsc" #-}
      revents <- (\hsc_ptr -> peekByteOff hsc_ptr 6) ptr
{-# LINE 138 "libraries/base/./System/Event/Poll.hsc" #-}
      let !pollFd' = PollFd fd events revents
      return pollFd'

    poke ptr p = do
      (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr (pfdFd p)
{-# LINE 143 "libraries/base/./System/Event/Poll.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 4) ptr (pfdEvents p)
{-# LINE 144 "libraries/base/./System/Event/Poll.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 6) ptr (pfdRevents p)
{-# LINE 145 "libraries/base/./System/Event/Poll.hsc" #-}

foreign import ccall safe "poll.h poll"
    c_poll :: Ptr PollFd -> CULong -> CInt -> IO CInt


{-# LINE 150 "libraries/base/./System/Event/Poll.hsc" #-}
