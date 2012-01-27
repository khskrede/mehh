{-# LINE 1 "libraries/unix/./System/Posix/Signals.hsc" #-}
{-# LANGUAGE DeriveDataTypeable,PatternGuards #-}
{-# LINE 2 "libraries/unix/./System/Posix/Signals.hsc" #-}
{-# OPTIONS_GHC -fno-cse #-} -- global variables
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Signals
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX signal support
--
-----------------------------------------------------------------------------


{-# LINE 18 "libraries/unix/./System/Posix/Signals.hsc" #-}
#include "HsUnixConfig.h"


{-# LINE 21 "libraries/unix/./System/Posix/Signals.hsc" #-}

{-# LINE 22 "libraries/unix/./System/Posix/Signals.hsc" #-}

{-# LINE 23 "libraries/unix/./System/Posix/Signals.hsc" #-}

module System.Posix.Signals (
  -- * The Signal type
  Signal,

  -- * Specific signals
  nullSignal,
  internalAbort, sigABRT,
  realTimeAlarm, sigALRM,
  busError, sigBUS,
  processStatusChanged, sigCHLD,
  continueProcess, sigCONT,
  floatingPointException, sigFPE,
  lostConnection, sigHUP,
  illegalInstruction, sigILL,
  keyboardSignal, sigINT,
  killProcess, sigKILL,
  openEndedPipe, sigPIPE,
  keyboardTermination, sigQUIT,
  segmentationViolation, sigSEGV,
  softwareStop, sigSTOP,
  softwareTermination, sigTERM,
  keyboardStop, sigTSTP,
  backgroundRead, sigTTIN,
  backgroundWrite, sigTTOU,
  userDefinedSignal1, sigUSR1,
  userDefinedSignal2, sigUSR2,

{-# LINE 51 "libraries/unix/./System/Posix/Signals.hsc" #-}
  pollableEvent, sigPOLL,

{-# LINE 53 "libraries/unix/./System/Posix/Signals.hsc" #-}
  profilingTimerExpired, sigPROF,
  badSystemCall, sigSYS,
  breakpointTrap, sigTRAP,
  urgentDataAvailable, sigURG,
  virtualTimerExpired, sigVTALRM,
  cpuTimeLimitExceeded, sigXCPU,
  fileSizeLimitExceeded, sigXFSZ,

  -- * Sending signals
  raiseSignal,
  signalProcess,
  signalProcessGroup,


{-# LINE 67 "libraries/unix/./System/Posix/Signals.hsc" #-}
  -- * Handling signals
  Handler(Default,Ignore,Catch,CatchOnce),
  installHandler,

{-# LINE 71 "libraries/unix/./System/Posix/Signals.hsc" #-}

  -- * Signal sets
  SignalSet,
  emptySignalSet, fullSignalSet, reservedSignals,
  addSignal, deleteSignal, inSignalSet,

  -- * The process signal mask
  getSignalMask, setSignalMask, blockSignals, unblockSignals,

  -- * The alarm timer
  scheduleAlarm,

  -- * Waiting for signals
  getPendingSignals,

{-# LINE 86 "libraries/unix/./System/Posix/Signals.hsc" #-}
  awaitSignal,

{-# LINE 88 "libraries/unix/./System/Posix/Signals.hsc" #-}


{-# LINE 90 "libraries/unix/./System/Posix/Signals.hsc" #-}
  -- * The @NOCLDSTOP@ flag
  setStoppedChildFlag, queryStoppedChildFlag,

{-# LINE 93 "libraries/unix/./System/Posix/Signals.hsc" #-}

  -- MISSING FUNCTIONALITY:
  -- sigaction(), (inc. the sigaction structure + flags etc.)
  -- the siginfo structure
  -- sigaltstack()
  -- sighold, sigignore, sigpause, sigrelse, sigset
  -- siginterrupt
  ) where

import Foreign hiding (unsafePerformIO)
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Types
import System.Posix.Internals
import System.Posix.Process
import System.Posix.Process.Internals
import Data.Dynamic


{-# LINE 112 "libraries/unix/./System/Posix/Signals.hsc" #-}

{-# LINE 113 "libraries/unix/./System/Posix/Signals.hsc" #-}
#include "rts/Signals.h"

{-# LINE 117 "libraries/unix/./System/Posix/Signals.hsc" #-}

import GHC.Conc hiding (Signal)

{-# LINE 120 "libraries/unix/./System/Posix/Signals.hsc" #-}

-- -----------------------------------------------------------------------------
-- Specific signals

nullSignal :: Signal
nullSignal = 0

sigABRT   :: CInt
sigABRT   = CONST_SIGABRT
sigALRM   :: CInt
sigALRM   = CONST_SIGALRM
sigBUS    :: CInt
sigBUS    = CONST_SIGBUS
sigCHLD   :: CInt
sigCHLD   = CONST_SIGCHLD
sigCONT   :: CInt
sigCONT   = CONST_SIGCONT
sigFPE    :: CInt
sigFPE    = CONST_SIGFPE
sigHUP    :: CInt
sigHUP    = CONST_SIGHUP
sigILL    :: CInt
sigILL    = CONST_SIGILL
sigINT    :: CInt
sigINT    = CONST_SIGINT
sigKILL   :: CInt
sigKILL   = CONST_SIGKILL
sigPIPE   :: CInt
sigPIPE   = CONST_SIGPIPE
sigQUIT   :: CInt
sigQUIT   = CONST_SIGQUIT
sigSEGV   :: CInt
sigSEGV   = CONST_SIGSEGV
sigSTOP   :: CInt
sigSTOP   = CONST_SIGSTOP
sigTERM   :: CInt
sigTERM   = CONST_SIGTERM
sigTSTP   :: CInt
sigTSTP   = CONST_SIGTSTP
sigTTIN   :: CInt
sigTTIN   = CONST_SIGTTIN
sigTTOU   :: CInt
sigTTOU   = CONST_SIGTTOU
sigUSR1   :: CInt
sigUSR1   = CONST_SIGUSR1
sigUSR2   :: CInt
sigUSR2   = CONST_SIGUSR2

{-# LINE 168 "libraries/unix/./System/Posix/Signals.hsc" #-}
sigPOLL   :: CInt
sigPOLL   = CONST_SIGPOLL

{-# LINE 171 "libraries/unix/./System/Posix/Signals.hsc" #-}
sigPROF   :: CInt
sigPROF   = CONST_SIGPROF
sigSYS    :: CInt
sigSYS    = CONST_SIGSYS
sigTRAP   :: CInt
sigTRAP   = CONST_SIGTRAP
sigURG    :: CInt
sigURG    = CONST_SIGURG
sigVTALRM :: CInt
sigVTALRM = CONST_SIGVTALRM
sigXCPU   :: CInt
sigXCPU   = CONST_SIGXCPU
sigXFSZ   :: CInt
sigXFSZ   = CONST_SIGXFSZ

internalAbort ::Signal
internalAbort = sigABRT

realTimeAlarm :: Signal
realTimeAlarm = sigALRM

busError :: Signal
busError = sigBUS

processStatusChanged :: Signal
processStatusChanged = sigCHLD

continueProcess :: Signal
continueProcess = sigCONT

floatingPointException :: Signal
floatingPointException = sigFPE

lostConnection :: Signal
lostConnection = sigHUP

illegalInstruction :: Signal
illegalInstruction = sigILL

keyboardSignal :: Signal
keyboardSignal = sigINT

killProcess :: Signal
killProcess = sigKILL

openEndedPipe :: Signal
openEndedPipe = sigPIPE

keyboardTermination :: Signal
keyboardTermination = sigQUIT

segmentationViolation :: Signal
segmentationViolation = sigSEGV

softwareStop :: Signal
softwareStop = sigSTOP

softwareTermination :: Signal
softwareTermination = sigTERM

keyboardStop :: Signal
keyboardStop = sigTSTP

backgroundRead :: Signal
backgroundRead = sigTTIN

backgroundWrite :: Signal
backgroundWrite = sigTTOU

userDefinedSignal1 :: Signal
userDefinedSignal1 = sigUSR1

userDefinedSignal2 :: Signal
userDefinedSignal2 = sigUSR2


{-# LINE 247 "libraries/unix/./System/Posix/Signals.hsc" #-}
pollableEvent :: Signal
pollableEvent = sigPOLL

{-# LINE 250 "libraries/unix/./System/Posix/Signals.hsc" #-}

profilingTimerExpired :: Signal
profilingTimerExpired = sigPROF

badSystemCall :: Signal
badSystemCall = sigSYS

breakpointTrap :: Signal
breakpointTrap = sigTRAP

urgentDataAvailable :: Signal
urgentDataAvailable = sigURG

virtualTimerExpired :: Signal
virtualTimerExpired = sigVTALRM

cpuTimeLimitExceeded :: Signal
cpuTimeLimitExceeded = sigXCPU

fileSizeLimitExceeded :: Signal
fileSizeLimitExceeded = sigXFSZ

-- -----------------------------------------------------------------------------
-- Signal-related functions

-- | @signalProcess int pid@ calls @kill@ to signal process @pid@ 
--   with interrupt signal @int@.
signalProcess :: Signal -> ProcessID -> IO ()
signalProcess sig pid 
 = throwErrnoIfMinus1_ "signalProcess" (c_kill (fromIntegral pid) sig)

foreign import ccall unsafe "kill"
  c_kill :: CPid -> CInt -> IO CInt


-- | @signalProcessGroup int pgid@ calls @kill@ to signal 
--  all processes in group @pgid@ with interrupt signal @int@.
signalProcessGroup :: Signal -> ProcessGroupID -> IO ()
signalProcessGroup sig pgid 
  = throwErrnoIfMinus1_ "signalProcessGroup" (c_killpg (fromIntegral pgid) sig)

foreign import ccall unsafe "killpg"
  c_killpg :: CPid -> CInt -> IO CInt

-- | @raiseSignal int@ calls @kill@ to signal the current process
--   with interrupt signal @int@. 
raiseSignal :: Signal -> IO ()
raiseSignal sig = throwErrnoIfMinus1_ "raiseSignal" (c_raise sig)


{-# LINE 303 "libraries/unix/./System/Posix/Signals.hsc" #-}
foreign import ccall unsafe "raise"
  c_raise :: CInt -> IO CInt

{-# LINE 306 "libraries/unix/./System/Posix/Signals.hsc" #-}


{-# LINE 308 "libraries/unix/./System/Posix/Signals.hsc" #-}
type Signal = CInt


-- | The actions to perform when a signal is received.
data Handler = Default
             | Ignore
	     -- not yet: | Hold 
             | Catch (IO ())
             | CatchOnce (IO ())
             | CatchInfo (SignalInfo -> IO ())
             | CatchInfoOnce (SignalInfo -> IO ())
  deriving (Typeable)

-- | Information about a received signal (derived from @siginfo_t@).
data SignalInfo = SignalInfo {
      siginfoSignal   :: Signal,
      siginfoError    :: Errno,
      siginfoSpecific :: SignalSpecificInfo
  }

-- | Information specific to a particular type of signal
-- (derived from @siginfo_t@).
data SignalSpecificInfo
  = NoSignalSpecificInfo
  | SigChldInfo {
      siginfoPid    :: ProcessID,
      siginfoUid    :: UserID,
      siginfoStatus :: ProcessStatus
    }

-- | @installHandler int handler iset@ calls @sigaction@ to install an
--   interrupt handler for signal @int@.  If @handler@ is @Default@,
--   @SIG_DFL@ is installed; if @handler@ is @Ignore@, @SIG_IGN@ is
--   installed; if @handler@ is @Catch action@, a handler is installed
--   which will invoke @action@ in a new thread when (or shortly after) the
--   signal is received.
--   If @iset@ is @Just s@, then the @sa_mask@ of the @sigaction@ structure
--   is set to @s@; otherwise it is cleared.  The previously installed
--   signal handler for @int@ is returned
installHandler :: Signal
               -> Handler
               -> Maybe SignalSet	-- ^ other signals to block
               -> IO Handler		-- ^ old handler


{-# LINE 356 "libraries/unix/./System/Posix/Signals.hsc" #-}

installHandler sig handler _maybe_mask = do
  ensureIOManagerIsRunning  -- for the threaded RTS

  -- if we're setting the action to DFL or IGN, we should do that *first*
  -- if we're setting a handler,
  --   if the previous action was handle, then setHandler is ok
  --   if the previous action was IGN/DFL, then setHandler followed by sig_install
  (old_action, old_handler) <-
    case handler of
      Ignore  -> do
        old_action  <- stg_sig_install sig STG_SIG_IGN nullPtr
        old_handler <- setHandler sig Nothing
        return (old_action, old_handler)

      Default -> do
        old_action  <- stg_sig_install sig STG_SIG_DFL nullPtr
        old_handler <- setHandler sig Nothing
        return (old_action, old_handler)

      _some_kind_of_catch -> do
        -- I don't think it's possible to get CatchOnce right.  If
        -- there's a signal in flight, then we might run the handler
        -- more than once.
        let dyn = toDyn handler
        old_handler <- case handler of
            Catch         action -> setHandler sig (Just (const action,dyn))
            CatchOnce     action -> setHandler sig (Just (const action,dyn))
            CatchInfo     action -> setHandler sig (Just (getinfo action,dyn))
            CatchInfoOnce action -> setHandler sig (Just (getinfo action,dyn))
            _                    -> error "installHandler"
            
        let action = case handler of
                Catch _         -> STG_SIG_HAN
                CatchOnce _     -> STG_SIG_RST
                CatchInfo _     -> STG_SIG_HAN
                CatchInfoOnce _ -> STG_SIG_RST
                _               -> error "installHandler"

        old_action <- stg_sig_install sig action nullPtr
                   -- mask is pointless, so leave it NULL

        return (old_action, old_handler)

  case (old_handler,old_action) of
    (_,       STG_SIG_DFL) -> return $ Default
    (_,       STG_SIG_IGN) -> return $ Ignore
    (Nothing, _)           -> return $ Ignore
    (Just (_,dyn),  _)
        | Just h <- fromDynamic dyn  -> return h
        | Just io <- fromDynamic dyn -> return (Catch io)
        -- handlers put there by the base package have type IO ()
        | otherwise                  -> return Default

foreign import ccall unsafe
  stg_sig_install
	:: CInt				-- sig no.
	-> CInt				-- action code (STG_SIG_HAN etc.)
	-> Ptr CSigset			-- (in, out) blocked
	-> IO CInt			-- (ret) old action code

getinfo :: (SignalInfo -> IO ()) -> ForeignPtr Word8 -> IO ()
getinfo handler fp_info = do
  si <- unmarshalSigInfo fp_info
  handler si

unmarshalSigInfo :: ForeignPtr Word8 -> IO SignalInfo
unmarshalSigInfo fp = do
  withForeignPtr fp $ \p -> do
    sig   <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) p
{-# LINE 426 "libraries/unix/./System/Posix/Signals.hsc" #-}
    errno <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) p
{-# LINE 427 "libraries/unix/./System/Posix/Signals.hsc" #-}
    extra <- case sig of
                _ | sig == sigCHLD -> do
                    pid <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) p
{-# LINE 430 "libraries/unix/./System/Posix/Signals.hsc" #-}
                    uid <- ((\hsc_ptr -> peekByteOff hsc_ptr 20)) p
{-# LINE 431 "libraries/unix/./System/Posix/Signals.hsc" #-}
                    wstat <- ((\hsc_ptr -> peekByteOff hsc_ptr 24)) p
{-# LINE 432 "libraries/unix/./System/Posix/Signals.hsc" #-}
                    pstat <- decipherWaitStatus wstat
                    return SigChldInfo { siginfoPid = pid,
                                         siginfoUid = uid,
                                         siginfoStatus = pstat }
                _ | otherwise ->
                    return NoSignalSpecificInfo
    return
      SignalInfo {
        siginfoSignal = sig,
        siginfoError  = Errno errno,
        siginfoSpecific = extra }


{-# LINE 445 "libraries/unix/./System/Posix/Signals.hsc" #-}

{-# LINE 446 "libraries/unix/./System/Posix/Signals.hsc" #-}

-- -----------------------------------------------------------------------------
-- Alarms

-- | @scheduleAlarm i@ calls @alarm@ to schedule a real time
--   alarm at least @i@ seconds in the future.
scheduleAlarm :: Int -> IO Int
scheduleAlarm secs = do
   r <- c_alarm (fromIntegral secs)
   return (fromIntegral r)

foreign import ccall unsafe "alarm"
  c_alarm :: CUInt -> IO CUInt


{-# LINE 461 "libraries/unix/./System/Posix/Signals.hsc" #-}
-- -----------------------------------------------------------------------------
-- The NOCLDSTOP flag

foreign import ccall "&nocldstop" nocldstop :: Ptr Int

-- | Tells the system whether or not to set the @SA_NOCLDSTOP@ flag when
-- installing new signal handlers.
setStoppedChildFlag :: Bool -> IO Bool
setStoppedChildFlag b = do
    rc <- peek nocldstop
    poke nocldstop $ fromEnum (not b) 
    return (rc == (0::Int))

-- | Queries the current state of the stopped child flag.
queryStoppedChildFlag :: IO Bool
queryStoppedChildFlag = do
    rc <- peek nocldstop
    return (rc == (0::Int))

{-# LINE 480 "libraries/unix/./System/Posix/Signals.hsc" #-}

-- -----------------------------------------------------------------------------
-- Manipulating signal sets

newtype SignalSet = SignalSet (ForeignPtr CSigset)

emptySignalSet :: SignalSet
emptySignalSet = unsafePerformIO $ do
  fp <- mallocForeignPtrBytes sizeof_sigset_t
  throwErrnoIfMinus1_ "emptySignalSet" (withForeignPtr fp $ c_sigemptyset)
  return (SignalSet fp)

fullSignalSet :: SignalSet
fullSignalSet = unsafePerformIO $ do
  fp <- mallocForeignPtrBytes sizeof_sigset_t
  throwErrnoIfMinus1_ "fullSignalSet" (withForeignPtr fp $ c_sigfillset)
  return (SignalSet fp)

-- | A set of signals reserved for use by the implementation.  In GHC, this will normally
-- include either `sigVTALRM` or `sigALRM`.
reservedSignals :: SignalSet
reservedSignals = addSignal rtsTimerSignal emptySignalSet

foreign import ccall rtsTimerSignal :: CInt

infixr `addSignal`, `deleteSignal`
addSignal :: Signal -> SignalSet -> SignalSet
addSignal sig (SignalSet fp1) = unsafePerformIO $ do
  fp2 <- mallocForeignPtrBytes sizeof_sigset_t
  withForeignPtr fp1 $ \p1 ->
    withForeignPtr fp2 $ \p2 -> do
      copyBytes p2 p1 sizeof_sigset_t
      throwErrnoIfMinus1_ "addSignal" (c_sigaddset p2 sig)
  return (SignalSet fp2)

deleteSignal :: Signal -> SignalSet -> SignalSet
deleteSignal sig (SignalSet fp1) = unsafePerformIO $ do
  fp2 <- mallocForeignPtrBytes sizeof_sigset_t
  withForeignPtr fp1 $ \p1 ->
    withForeignPtr fp2 $ \p2 -> do
      copyBytes p2 p1 sizeof_sigset_t
      throwErrnoIfMinus1_ "deleteSignal" (c_sigdelset p2 sig)
  return (SignalSet fp2)

inSignalSet :: Signal -> SignalSet -> Bool
inSignalSet sig (SignalSet fp) = unsafePerformIO $
  withForeignPtr fp $ \p -> do
    r <- throwErrnoIfMinus1 "inSignalSet" (c_sigismember p sig)
    return (r /= 0)

-- | @getSignalMask@ calls @sigprocmask@ to determine the
--   set of interrupts which are currently being blocked.
getSignalMask :: IO SignalSet
getSignalMask = do
  fp <- mallocForeignPtrBytes sizeof_sigset_t
  withForeignPtr fp $ \p ->
    throwErrnoIfMinus1_ "getSignalMask" (c_sigprocmask 0 nullPtr p)
  return (SignalSet fp)
   
sigProcMask :: String -> CInt -> SignalSet -> IO ()
sigProcMask fn how (SignalSet set) =
  withForeignPtr set $ \p_set ->
    throwErrnoIfMinus1_ fn (c_sigprocmask how p_set nullPtr)

-- | @setSignalMask mask@ calls @sigprocmask@ with
--   @SIG_SETMASK@ to block all interrupts in @mask@.
setSignalMask :: SignalSet -> IO ()
setSignalMask set = sigProcMask "setSignalMask" (CONST_SIG_SETMASK :: CInt) set

-- | @blockSignals mask@ calls @sigprocmask@ with
--   @SIG_BLOCK@ to add all interrupts in @mask@ to the
--  set of blocked interrupts.
blockSignals :: SignalSet -> IO ()
blockSignals set = sigProcMask "blockSignals" (CONST_SIG_BLOCK :: CInt) set

-- | @unblockSignals mask@ calls @sigprocmask@ with
--   @SIG_UNBLOCK@ to remove all interrupts in @mask@ from the
--   set of blocked interrupts. 
unblockSignals :: SignalSet -> IO ()
unblockSignals set = sigProcMask "unblockSignals" (CONST_SIG_UNBLOCK :: CInt) set

-- | @getPendingSignals@ calls @sigpending@ to obtain
--   the set of interrupts which have been received but are currently blocked.
getPendingSignals :: IO SignalSet
getPendingSignals = do
  fp <- mallocForeignPtrBytes sizeof_sigset_t
  withForeignPtr fp $ \p -> 
   throwErrnoIfMinus1_ "getPendingSignals" (c_sigpending p)
  return (SignalSet fp)


{-# LINE 571 "libraries/unix/./System/Posix/Signals.hsc" #-}

-- | @awaitSignal iset@ suspends execution until an interrupt is received.
-- If @iset@ is @Just s@, @awaitSignal@ calls @sigsuspend@, installing
-- @s@ as the new signal mask before suspending execution; otherwise, it
-- calls @sigsuspend@ with current signal mask. Note that RTS
-- scheduler signal (either 'virtualTimerExpired' or 'realTimeAlarm') 
-- could cause premature termination of this call. It might be necessary to block that
-- signal before invocation of @awaitSignal@ with 'blockSignals' 'reservedSignals'.
--
-- @awaitSignal@ returns when signal was received and processed by a
-- signal handler, or if the signal could not be caught. If you have
-- installed any signal handlers with @installHandler@, it may be wise
-- to call @yield@ directly after @awaitSignal@ to ensure that the
-- signal handler runs as promptly as possible.
awaitSignal :: Maybe SignalSet -> IO ()
awaitSignal maybe_sigset = do
  fp <- case maybe_sigset of
    	  Nothing -> do SignalSet fp <- getSignalMask; return fp
    	  Just (SignalSet fp) -> return fp
  withForeignPtr fp $ \p -> do
  _ <- c_sigsuspend p
  return ()
  -- ignore the return value; according to the docs it can only ever be
  -- (-1) with errno set to EINTR.
  -- XXX My manpage says it can also return EFAULT. And why is ignoring
  -- EINTR the right thing to do?
 
foreign import ccall unsafe "sigsuspend"
  c_sigsuspend :: Ptr CSigset -> IO CInt

{-# LINE 601 "libraries/unix/./System/Posix/Signals.hsc" #-}


{-# LINE 612 "libraries/unix/./System/Posix/Signals.hsc" #-}
foreign import ccall unsafe "__hscore_sigdelset"
  c_sigdelset   :: Ptr CSigset -> CInt -> IO CInt

foreign import ccall unsafe "__hscore_sigfillset"
  c_sigfillset  :: Ptr CSigset -> IO CInt

foreign import ccall unsafe "__hscore_sigismember"
  c_sigismember :: Ptr CSigset -> CInt -> IO CInt

{-# LINE 621 "libraries/unix/./System/Posix/Signals.hsc" #-}

foreign import ccall unsafe "sigpending"
  c_sigpending :: Ptr CSigset -> IO CInt

