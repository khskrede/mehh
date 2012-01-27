{-# LINE 1 "libraries/unix/./System/Posix/Terminal.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LINE 2 "libraries/unix/./System/Posix/Terminal.hsc" #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Terminal
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX Terminal support
--
-----------------------------------------------------------------------------

module System.Posix.Terminal (
  -- * Terminal support

  -- ** Terminal attributes
  TerminalAttributes,
  getTerminalAttributes,
  TerminalState(..),
  setTerminalAttributes,

  TerminalMode(..),
  withoutMode,
  withMode,
  terminalMode,
  bitsPerByte,
  withBits,

  ControlCharacter(..),
  controlChar,
  withCC,
  withoutCC,

  inputTime,
  withTime,
  minInput,
  withMinInput,

  BaudRate(..),
  inputSpeed,
  withInputSpeed,
  outputSpeed,
  withOutputSpeed,

  -- ** Terminal operations
  sendBreak,
  drainOutput,
  QueueSelector(..),
  discardData,
  FlowAction(..),
  controlFlow,

  -- ** Process groups
  getTerminalProcessGroupID,
  setTerminalProcessGroupID,

  -- ** Testing a file descriptor
  queryTerminal,
  getTerminalName,
  getControllingTerminalName,

  -- ** Pseudoterminal operations
  openPseudoTerminal,
  getSlaveTerminalName
  ) where


{-# LINE 72 "libraries/unix/./System/Posix/Terminal.hsc" #-}

import Data.Bits
import Data.Char
import Foreign.C.Error ( errnoToIOError, throwErrnoIfMinus1,
                         throwErrnoIfMinus1_, throwErrnoIfNull )

{-# LINE 80 "libraries/unix/./System/Posix/Terminal.hsc" #-}
import Foreign.C.String ( CString, peekCString, withCString )
import Foreign.C.Types ( CInt )
import Foreign.ForeignPtr ( ForeignPtr, withForeignPtr, mallocForeignPtrBytes )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Utils ( copyBytes )
import Foreign.Ptr ( Ptr, nullPtr, plusPtr )
import Foreign.Storable ( Storable(..) )
import System.IO.Error ( ioError )
import System.IO.Unsafe ( unsafePerformIO )
import System.Posix.IO ( OpenFileFlags(..), OpenMode(..), defaultFileFlags,
                         openFd )
import System.Posix.Types

-- -----------------------------------------------------------------------------
-- Terminal attributes

type CTermios = ()
newtype TerminalAttributes = TerminalAttributes (ForeignPtr CTermios)

makeTerminalAttributes :: ForeignPtr CTermios -> TerminalAttributes
makeTerminalAttributes = TerminalAttributes

withTerminalAttributes :: TerminalAttributes -> (Ptr CTermios -> IO a) -> IO a
withTerminalAttributes (TerminalAttributes termios) = withForeignPtr termios


data TerminalMode
	-- input flags
   = InterruptOnBreak		-- BRKINT
   | MapCRtoLF			-- ICRNL
   | IgnoreBreak		-- IGNBRK
   | IgnoreCR			-- IGNCR
   | IgnoreParityErrors		-- IGNPAR
   | MapLFtoCR			-- INLCR
   | CheckParity		-- INPCK
   | StripHighBit		-- ISTRIP
   | StartStopInput		-- IXOFF
   | StartStopOutput		-- IXON
   | MarkParityErrors		-- PARMRK

	-- output flags
   | ProcessOutput		-- OPOST
	-- ToDo: ONLCR, OCRNL, ONOCR, ONLRET, OFILL,
	--       NLDLY(NL0,NL1), CRDLY(CR0,CR1,CR2,CR2)
	--	 TABDLY(TAB0,TAB1,TAB2,TAB3)
	--	 BSDLY(BS0,BS1), VTDLY(VT0,VT1), FFDLY(FF0,FF1)

	-- control flags
   | LocalMode			-- CLOCAL
   | ReadEnable			-- CREAD
   | TwoStopBits		-- CSTOPB
   | HangupOnClose		-- HUPCL
   | EnableParity		-- PARENB
   | OddParity			-- PARODD

	-- local modes
   | EnableEcho			-- ECHO
   | EchoErase			-- ECHOE
   | EchoKill			-- ECHOK
   | EchoLF			-- ECHONL
   | ProcessInput		-- ICANON
   | ExtendedFunctions		-- IEXTEN
   | KeyboardInterrupts		-- ISIG
   | NoFlushOnInterrupt		-- NOFLSH
   | BackgroundWriteInterrupt	-- TOSTOP

withoutMode :: TerminalAttributes -> TerminalMode -> TerminalAttributes
withoutMode termios InterruptOnBreak = clearInputFlag (2) termios
{-# LINE 148 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withoutMode termios MapCRtoLF = clearInputFlag (256) termios
{-# LINE 149 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withoutMode termios IgnoreBreak = clearInputFlag (1) termios
{-# LINE 150 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withoutMode termios IgnoreCR = clearInputFlag (128) termios
{-# LINE 151 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withoutMode termios IgnoreParityErrors = clearInputFlag (4) termios
{-# LINE 152 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withoutMode termios MapLFtoCR = clearInputFlag (64) termios
{-# LINE 153 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withoutMode termios CheckParity = clearInputFlag (16) termios
{-# LINE 154 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withoutMode termios StripHighBit = clearInputFlag (32) termios
{-# LINE 155 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withoutMode termios StartStopInput = clearInputFlag (4096) termios
{-# LINE 156 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withoutMode termios StartStopOutput = clearInputFlag (1024) termios
{-# LINE 157 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withoutMode termios MarkParityErrors = clearInputFlag (8) termios
{-# LINE 158 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withoutMode termios ProcessOutput = clearOutputFlag (1) termios
{-# LINE 159 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withoutMode termios LocalMode = clearControlFlag (2048) termios
{-# LINE 160 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withoutMode termios ReadEnable = clearControlFlag (128) termios
{-# LINE 161 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withoutMode termios TwoStopBits = clearControlFlag (64) termios
{-# LINE 162 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withoutMode termios HangupOnClose = clearControlFlag (1024) termios
{-# LINE 163 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withoutMode termios EnableParity = clearControlFlag (256) termios
{-# LINE 164 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withoutMode termios OddParity = clearControlFlag (512) termios
{-# LINE 165 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withoutMode termios EnableEcho = clearLocalFlag (8) termios
{-# LINE 166 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withoutMode termios EchoErase = clearLocalFlag (16) termios
{-# LINE 167 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withoutMode termios EchoKill = clearLocalFlag (32) termios
{-# LINE 168 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withoutMode termios EchoLF = clearLocalFlag (64) termios
{-# LINE 169 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withoutMode termios ProcessInput = clearLocalFlag (2) termios
{-# LINE 170 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withoutMode termios ExtendedFunctions = clearLocalFlag (32768) termios
{-# LINE 171 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withoutMode termios KeyboardInterrupts = clearLocalFlag (1) termios
{-# LINE 172 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withoutMode termios NoFlushOnInterrupt = setLocalFlag (128) termios
{-# LINE 173 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withoutMode termios BackgroundWriteInterrupt = clearLocalFlag (256) termios
{-# LINE 174 "libraries/unix/./System/Posix/Terminal.hsc" #-}

withMode :: TerminalAttributes -> TerminalMode -> TerminalAttributes
withMode termios InterruptOnBreak = setInputFlag (2) termios
{-# LINE 177 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withMode termios MapCRtoLF = setInputFlag (256) termios
{-# LINE 178 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withMode termios IgnoreBreak = setInputFlag (1) termios
{-# LINE 179 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withMode termios IgnoreCR = setInputFlag (128) termios
{-# LINE 180 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withMode termios IgnoreParityErrors = setInputFlag (4) termios
{-# LINE 181 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withMode termios MapLFtoCR = setInputFlag (64) termios
{-# LINE 182 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withMode termios CheckParity = setInputFlag (16) termios
{-# LINE 183 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withMode termios StripHighBit = setInputFlag (32) termios
{-# LINE 184 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withMode termios StartStopInput = setInputFlag (4096) termios
{-# LINE 185 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withMode termios StartStopOutput = setInputFlag (1024) termios
{-# LINE 186 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withMode termios MarkParityErrors = setInputFlag (8) termios
{-# LINE 187 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withMode termios ProcessOutput = setOutputFlag (1) termios
{-# LINE 188 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withMode termios LocalMode = setControlFlag (2048) termios
{-# LINE 189 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withMode termios ReadEnable = setControlFlag (128) termios
{-# LINE 190 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withMode termios TwoStopBits = setControlFlag (64) termios
{-# LINE 191 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withMode termios HangupOnClose = setControlFlag (1024) termios
{-# LINE 192 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withMode termios EnableParity = setControlFlag (256) termios
{-# LINE 193 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withMode termios OddParity = setControlFlag (512) termios
{-# LINE 194 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withMode termios EnableEcho = setLocalFlag (8) termios
{-# LINE 195 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withMode termios EchoErase = setLocalFlag (16) termios
{-# LINE 196 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withMode termios EchoKill = setLocalFlag (32) termios
{-# LINE 197 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withMode termios EchoLF = setLocalFlag (64) termios
{-# LINE 198 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withMode termios ProcessInput = setLocalFlag (2) termios
{-# LINE 199 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withMode termios ExtendedFunctions = setLocalFlag (32768) termios
{-# LINE 200 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withMode termios KeyboardInterrupts = setLocalFlag (1) termios
{-# LINE 201 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withMode termios NoFlushOnInterrupt = clearLocalFlag (128) termios
{-# LINE 202 "libraries/unix/./System/Posix/Terminal.hsc" #-}
withMode termios BackgroundWriteInterrupt = setLocalFlag (256) termios
{-# LINE 203 "libraries/unix/./System/Posix/Terminal.hsc" #-}

terminalMode :: TerminalMode -> TerminalAttributes -> Bool
terminalMode InterruptOnBreak = testInputFlag (2)
{-# LINE 206 "libraries/unix/./System/Posix/Terminal.hsc" #-}
terminalMode MapCRtoLF = testInputFlag (256)
{-# LINE 207 "libraries/unix/./System/Posix/Terminal.hsc" #-}
terminalMode IgnoreBreak = testInputFlag (1)
{-# LINE 208 "libraries/unix/./System/Posix/Terminal.hsc" #-}
terminalMode IgnoreCR = testInputFlag (128)
{-# LINE 209 "libraries/unix/./System/Posix/Terminal.hsc" #-}
terminalMode IgnoreParityErrors = testInputFlag (4)
{-# LINE 210 "libraries/unix/./System/Posix/Terminal.hsc" #-}
terminalMode MapLFtoCR = testInputFlag (64)
{-# LINE 211 "libraries/unix/./System/Posix/Terminal.hsc" #-}
terminalMode CheckParity = testInputFlag (16)
{-# LINE 212 "libraries/unix/./System/Posix/Terminal.hsc" #-}
terminalMode StripHighBit = testInputFlag (32)
{-# LINE 213 "libraries/unix/./System/Posix/Terminal.hsc" #-}
terminalMode StartStopInput = testInputFlag (4096)
{-# LINE 214 "libraries/unix/./System/Posix/Terminal.hsc" #-}
terminalMode StartStopOutput = testInputFlag (1024)
{-# LINE 215 "libraries/unix/./System/Posix/Terminal.hsc" #-}
terminalMode MarkParityErrors = testInputFlag (8)
{-# LINE 216 "libraries/unix/./System/Posix/Terminal.hsc" #-}
terminalMode ProcessOutput = testOutputFlag (1)
{-# LINE 217 "libraries/unix/./System/Posix/Terminal.hsc" #-}
terminalMode LocalMode = testControlFlag (2048)
{-# LINE 218 "libraries/unix/./System/Posix/Terminal.hsc" #-}
terminalMode ReadEnable = testControlFlag (128)
{-# LINE 219 "libraries/unix/./System/Posix/Terminal.hsc" #-}
terminalMode TwoStopBits = testControlFlag (64)
{-# LINE 220 "libraries/unix/./System/Posix/Terminal.hsc" #-}
terminalMode HangupOnClose = testControlFlag (1024)
{-# LINE 221 "libraries/unix/./System/Posix/Terminal.hsc" #-}
terminalMode EnableParity = testControlFlag (256)
{-# LINE 222 "libraries/unix/./System/Posix/Terminal.hsc" #-}
terminalMode OddParity = testControlFlag (512)
{-# LINE 223 "libraries/unix/./System/Posix/Terminal.hsc" #-}
terminalMode EnableEcho = testLocalFlag (8)
{-# LINE 224 "libraries/unix/./System/Posix/Terminal.hsc" #-}
terminalMode EchoErase = testLocalFlag (16)
{-# LINE 225 "libraries/unix/./System/Posix/Terminal.hsc" #-}
terminalMode EchoKill = testLocalFlag (32)
{-# LINE 226 "libraries/unix/./System/Posix/Terminal.hsc" #-}
terminalMode EchoLF = testLocalFlag (64)
{-# LINE 227 "libraries/unix/./System/Posix/Terminal.hsc" #-}
terminalMode ProcessInput = testLocalFlag (2)
{-# LINE 228 "libraries/unix/./System/Posix/Terminal.hsc" #-}
terminalMode ExtendedFunctions = testLocalFlag (32768)
{-# LINE 229 "libraries/unix/./System/Posix/Terminal.hsc" #-}
terminalMode KeyboardInterrupts = testLocalFlag (1)
{-# LINE 230 "libraries/unix/./System/Posix/Terminal.hsc" #-}
terminalMode NoFlushOnInterrupt = not . testLocalFlag (128)
{-# LINE 231 "libraries/unix/./System/Posix/Terminal.hsc" #-}
terminalMode BackgroundWriteInterrupt = testLocalFlag (256)
{-# LINE 232 "libraries/unix/./System/Posix/Terminal.hsc" #-}

bitsPerByte :: TerminalAttributes -> Int
bitsPerByte termios = unsafePerformIO $ do
  withTerminalAttributes termios $ \p -> do
    cflag <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) p
{-# LINE 237 "libraries/unix/./System/Posix/Terminal.hsc" #-}
    return $! (word2Bits (cflag .&. (48)))
{-# LINE 238 "libraries/unix/./System/Posix/Terminal.hsc" #-}
  where
    word2Bits :: CTcflag -> Int
    word2Bits x =
	if x == (0) then 5
{-# LINE 242 "libraries/unix/./System/Posix/Terminal.hsc" #-}
	else if x == (16) then 6
{-# LINE 243 "libraries/unix/./System/Posix/Terminal.hsc" #-}
	else if x == (32) then 7
{-# LINE 244 "libraries/unix/./System/Posix/Terminal.hsc" #-}
	else if x == (48) then 8
{-# LINE 245 "libraries/unix/./System/Posix/Terminal.hsc" #-}
	else 0

withBits :: TerminalAttributes -> Int -> TerminalAttributes
withBits termios bits = unsafePerformIO $ do
  withNewTermios termios $ \p -> do
    cflag <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) p
{-# LINE 251 "libraries/unix/./System/Posix/Terminal.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) p
{-# LINE 252 "libraries/unix/./System/Posix/Terminal.hsc" #-}
       ((cflag .&. complement (48)) .|. mask bits)
{-# LINE 253 "libraries/unix/./System/Posix/Terminal.hsc" #-}
  where
    mask :: Int -> CTcflag
    mask 5 = (0)
{-# LINE 256 "libraries/unix/./System/Posix/Terminal.hsc" #-}
    mask 6 = (16)
{-# LINE 257 "libraries/unix/./System/Posix/Terminal.hsc" #-}
    mask 7 = (32)
{-# LINE 258 "libraries/unix/./System/Posix/Terminal.hsc" #-}
    mask 8 = (48)
{-# LINE 259 "libraries/unix/./System/Posix/Terminal.hsc" #-}
    mask _ = error "withBits bit value out of range [5..8]"

data ControlCharacter
  = EndOfFile		-- VEOF
  | EndOfLine		-- VEOL
  | Erase		-- VERASE
  | Interrupt		-- VINTR
  | Kill		-- VKILL
  | Quit		-- VQUIT
  | Start		-- VSTART
  | Stop		-- VSTOP
  | Suspend		-- VSUSP

controlChar :: TerminalAttributes -> ControlCharacter -> Maybe Char
controlChar termios cc = unsafePerformIO $ do
  withTerminalAttributes termios $ \p -> do
    let c_cc = ((\hsc_ptr -> hsc_ptr `plusPtr` 17)) p
{-# LINE 276 "libraries/unix/./System/Posix/Terminal.hsc" #-}
    val <- peekElemOff c_cc (cc2Word cc)
    if val == ((0)::CCc)
{-# LINE 278 "libraries/unix/./System/Posix/Terminal.hsc" #-}
       then return Nothing
       else return (Just (chr (fromEnum val)))
  
withCC :: TerminalAttributes
       -> (ControlCharacter, Char)
       -> TerminalAttributes
withCC termios (cc, c) = unsafePerformIO $ do
  withNewTermios termios $ \p -> do
    let c_cc = ((\hsc_ptr -> hsc_ptr `plusPtr` 17)) p
{-# LINE 287 "libraries/unix/./System/Posix/Terminal.hsc" #-}
    pokeElemOff c_cc (cc2Word cc) (fromIntegral (ord c) :: CCc)

withoutCC :: TerminalAttributes
          -> ControlCharacter
          -> TerminalAttributes
withoutCC termios cc = unsafePerformIO $ do
  withNewTermios termios $ \p -> do
    let c_cc = ((\hsc_ptr -> hsc_ptr `plusPtr` 17)) p
{-# LINE 295 "libraries/unix/./System/Posix/Terminal.hsc" #-}
    pokeElemOff c_cc (cc2Word cc) ((0) :: CCc)
{-# LINE 296 "libraries/unix/./System/Posix/Terminal.hsc" #-}

inputTime :: TerminalAttributes -> Int
inputTime termios = unsafePerformIO $ do
  withTerminalAttributes termios $ \p -> do
    c <- peekElemOff (((\hsc_ptr -> hsc_ptr `plusPtr` 17)) p) (5)
{-# LINE 301 "libraries/unix/./System/Posix/Terminal.hsc" #-}
    return (fromEnum (c :: CCc))

withTime :: TerminalAttributes -> Int -> TerminalAttributes
withTime termios time = unsafePerformIO $ do
  withNewTermios termios $ \p -> do
    let c_cc = ((\hsc_ptr -> hsc_ptr `plusPtr` 17)) p
{-# LINE 307 "libraries/unix/./System/Posix/Terminal.hsc" #-}
    pokeElemOff c_cc (5) (fromIntegral time :: CCc)
{-# LINE 308 "libraries/unix/./System/Posix/Terminal.hsc" #-}

minInput :: TerminalAttributes -> Int
minInput termios = unsafePerformIO $ do
  withTerminalAttributes termios $ \p -> do
    c <- peekElemOff (((\hsc_ptr -> hsc_ptr `plusPtr` 17)) p) (6)
{-# LINE 313 "libraries/unix/./System/Posix/Terminal.hsc" #-}
    return (fromEnum (c :: CCc))

withMinInput :: TerminalAttributes -> Int -> TerminalAttributes
withMinInput termios count = unsafePerformIO $ do
  withNewTermios termios $ \p -> do
    let c_cc = ((\hsc_ptr -> hsc_ptr `plusPtr` 17)) p
{-# LINE 319 "libraries/unix/./System/Posix/Terminal.hsc" #-}
    pokeElemOff c_cc (6) (fromIntegral count :: CCc)
{-# LINE 320 "libraries/unix/./System/Posix/Terminal.hsc" #-}

data BaudRate
  = B0
  | B50
  | B75
  | B110
  | B134
  | B150
  | B200
  | B300
  | B600
  | B1200
  | B1800
  | B2400
  | B4800
  | B9600
  | B19200
  | B38400
  | B57600
  | B115200

inputSpeed :: TerminalAttributes -> BaudRate
inputSpeed termios = unsafePerformIO $ do
  withTerminalAttributes termios $ \p -> do
    w <- c_cfgetispeed p
    return (word2Baud w)

foreign import ccall unsafe "cfgetispeed"
  c_cfgetispeed :: Ptr CTermios -> IO CSpeed

withInputSpeed :: TerminalAttributes -> BaudRate -> TerminalAttributes
withInputSpeed termios br = unsafePerformIO $ do
  withNewTermios termios $ \p -> c_cfsetispeed p (baud2Word br)

foreign import ccall unsafe "cfsetispeed"
  c_cfsetispeed :: Ptr CTermios -> CSpeed -> IO CInt


outputSpeed :: TerminalAttributes -> BaudRate
outputSpeed termios = unsafePerformIO $ do
  withTerminalAttributes termios $ \p ->  do
    w <- c_cfgetospeed p
    return (word2Baud w)

foreign import ccall unsafe "cfgetospeed"
  c_cfgetospeed :: Ptr CTermios -> IO CSpeed

withOutputSpeed :: TerminalAttributes -> BaudRate -> TerminalAttributes
withOutputSpeed termios br = unsafePerformIO $ do
  withNewTermios termios $ \p -> c_cfsetospeed p (baud2Word br)

foreign import ccall unsafe "cfsetospeed"
  c_cfsetospeed :: Ptr CTermios -> CSpeed -> IO CInt

-- | @getTerminalAttributes fd@ calls @tcgetattr@ to obtain
--   the @TerminalAttributes@ associated with @Fd@ @fd@.
getTerminalAttributes :: Fd -> IO TerminalAttributes
getTerminalAttributes (Fd fd) = do
  fp <- mallocForeignPtrBytes (60)
{-# LINE 379 "libraries/unix/./System/Posix/Terminal.hsc" #-}
  withForeignPtr fp $ \p ->
      throwErrnoIfMinus1_ "getTerminalAttributes" (c_tcgetattr fd p)
  return $ makeTerminalAttributes fp

foreign import ccall unsafe "tcgetattr"
  c_tcgetattr :: CInt -> Ptr CTermios -> IO CInt

data TerminalState
  = Immediately
  | WhenDrained
  | WhenFlushed

-- | @setTerminalAttributes fd attr ts@ calls @tcsetattr@ to change
--   the @TerminalAttributes@ associated with @Fd@ @fd@ to
--   @attr@, when the terminal is in the state indicated by @ts@.
setTerminalAttributes :: Fd
                      -> TerminalAttributes
                      -> TerminalState
                      -> IO ()
setTerminalAttributes (Fd fd) termios state = do
  withTerminalAttributes termios $ \p ->
    throwErrnoIfMinus1_ "setTerminalAttributes"
      (c_tcsetattr fd (state2Int state) p)
  where
    state2Int :: TerminalState -> CInt
    state2Int Immediately = (0)
{-# LINE 405 "libraries/unix/./System/Posix/Terminal.hsc" #-}
    state2Int WhenDrained = (1)
{-# LINE 406 "libraries/unix/./System/Posix/Terminal.hsc" #-}
    state2Int WhenFlushed = (2)
{-# LINE 407 "libraries/unix/./System/Posix/Terminal.hsc" #-}

foreign import ccall unsafe "tcsetattr"
   c_tcsetattr :: CInt -> CInt -> Ptr CTermios -> IO CInt

-- | @sendBreak fd duration@ calls @tcsendbreak@ to transmit a
--   continuous stream of zero-valued bits on @Fd@ @fd@ for the
--   specified implementation-dependent @duration@.
sendBreak :: Fd -> Int -> IO ()
sendBreak (Fd fd) duration
  = throwErrnoIfMinus1_ "sendBreak" (c_tcsendbreak fd (fromIntegral duration))

foreign import ccall unsafe "tcsendbreak"
  c_tcsendbreak :: CInt -> CInt -> IO CInt

-- | @drainOutput fd@ calls @tcdrain@ to block until all output
--   written to @Fd@ @fd@ has been transmitted.
drainOutput :: Fd -> IO ()
drainOutput (Fd fd) = throwErrnoIfMinus1_ "drainOutput" (c_tcdrain fd)

foreign import ccall unsafe "tcdrain"
  c_tcdrain :: CInt -> IO CInt


data QueueSelector
  = InputQueue		-- TCIFLUSH
  | OutputQueue		-- TCOFLUSH
  | BothQueues		-- TCIOFLUSH

-- | @discardData fd queues@ calls @tcflush@ to discard
--   pending input and\/or output for @Fd@ @fd@,
--   as indicated by the @QueueSelector@ @queues@.
discardData :: Fd -> QueueSelector -> IO ()
discardData (Fd fd) queue =
  throwErrnoIfMinus1_ "discardData" (c_tcflush fd (queue2Int queue))
  where
    queue2Int :: QueueSelector -> CInt
    queue2Int InputQueue  = (0)
{-# LINE 444 "libraries/unix/./System/Posix/Terminal.hsc" #-}
    queue2Int OutputQueue = (1)
{-# LINE 445 "libraries/unix/./System/Posix/Terminal.hsc" #-}
    queue2Int BothQueues  = (2)
{-# LINE 446 "libraries/unix/./System/Posix/Terminal.hsc" #-}

foreign import ccall unsafe "tcflush"
  c_tcflush :: CInt -> CInt -> IO CInt

data FlowAction
  = SuspendOutput	-- ^ TCOOFF
  | RestartOutput	-- ^ TCOON
  | TransmitStop	-- ^ TCIOFF
  | TransmitStart	-- ^ TCION

-- | @controlFlow fd action@ calls @tcflow@ to control the 
--   flow of data on @Fd@ @fd@, as indicated by
--   @action@.
controlFlow :: Fd -> FlowAction -> IO ()
controlFlow (Fd fd) action =
  throwErrnoIfMinus1_ "controlFlow" (c_tcflow fd (action2Int action))
  where
    action2Int :: FlowAction -> CInt
    action2Int SuspendOutput = (0)
{-# LINE 465 "libraries/unix/./System/Posix/Terminal.hsc" #-}
    action2Int RestartOutput = (1)
{-# LINE 466 "libraries/unix/./System/Posix/Terminal.hsc" #-}
    action2Int TransmitStop  = (2)
{-# LINE 467 "libraries/unix/./System/Posix/Terminal.hsc" #-}
    action2Int TransmitStart = (3)
{-# LINE 468 "libraries/unix/./System/Posix/Terminal.hsc" #-}

foreign import ccall unsafe "tcflow"
  c_tcflow :: CInt -> CInt -> IO CInt

-- | @getTerminalProcessGroupID fd@ calls @tcgetpgrp@ to
--   obtain the @ProcessGroupID@ of the foreground process group 
--   associated with the terminal attached to @Fd@ @fd@.
getTerminalProcessGroupID :: Fd -> IO ProcessGroupID
getTerminalProcessGroupID (Fd fd) = do
  throwErrnoIfMinus1 "getTerminalProcessGroupID" (c_tcgetpgrp fd)

foreign import ccall unsafe "tcgetpgrp"
  c_tcgetpgrp :: CInt -> IO CPid

-- | @setTerminalProcessGroupID fd pgid@ calls @tcsetpgrp@ to
--   set the @ProcessGroupID@ of the foreground process group 
--   associated with the terminal attached to @Fd@ 
--   @fd@ to @pgid@.
setTerminalProcessGroupID :: Fd -> ProcessGroupID -> IO ()
setTerminalProcessGroupID (Fd fd) pgid =
  throwErrnoIfMinus1_ "setTerminalProcessGroupID" (c_tcsetpgrp fd pgid)

foreign import ccall unsafe "tcsetpgrp"
  c_tcsetpgrp :: CInt -> CPid -> IO CInt

-- -----------------------------------------------------------------------------
-- file descriptor queries

-- | @queryTerminal fd@ calls @isatty@ to determine whether or
--   not @Fd@ @fd@ is associated with a terminal.
queryTerminal :: Fd -> IO Bool
queryTerminal (Fd fd) = do
  r <- c_isatty fd
  return (r == 1)
  -- ToDo: the spec says that it can set errno to EBADF if the result is zero

foreign import ccall unsafe "isatty"
  c_isatty :: CInt -> IO CInt

-- | @getTerminalName fd@ calls @ttyname@ to obtain a name associated
--   with the terminal for @Fd@ @fd@. If @fd@ is associated
--   with a terminal, @getTerminalName@ returns the name of the
--   terminal.
getTerminalName :: Fd -> IO FilePath
getTerminalName (Fd fd) = do
  s <- throwErrnoIfNull "getTerminalName" (c_ttyname fd)
  peekCString s  

foreign import ccall unsafe "ttyname"
  c_ttyname :: CInt -> IO CString

-- | @getControllingTerminalName@ calls @ctermid@ to obtain
--   a name associated with the controlling terminal for the process.  If a
--   controlling terminal exists,
--   @getControllingTerminalName@ returns the name of the
--   controlling terminal.
getControllingTerminalName :: IO FilePath
getControllingTerminalName = do
  s <- throwErrnoIfNull "getControllingTerminalName" (c_ctermid nullPtr)
  peekCString s

foreign import ccall unsafe "ctermid"
  c_ctermid :: CString -> IO CString

-- | @getSlaveTerminalName@ calls @ptsname@ to obtain the name of the
-- slave terminal associated with a pseudoterminal pair.  The file
-- descriptor to pass in must be that of the master.
getSlaveTerminalName :: Fd -> IO FilePath


{-# LINE 538 "libraries/unix/./System/Posix/Terminal.hsc" #-}
getSlaveTerminalName (Fd fd) = do
  s <- throwErrnoIfNull "getSlaveTerminalName" (c_ptsname fd)
  peekCString s

foreign import ccall unsafe "__hsunix_ptsname"
  c_ptsname :: CInt -> IO CString

{-# LINE 548 "libraries/unix/./System/Posix/Terminal.hsc" #-}

-- | @openPseudoTerminal@ creates a pseudoterminal (pty) pair, and
-- returns the newly created pair as a (@master@, @slave@) tuple.
openPseudoTerminal :: IO (Fd, Fd)


{-# LINE 554 "libraries/unix/./System/Posix/Terminal.hsc" #-}
openPseudoTerminal =
  alloca $ \p_master ->
    alloca $ \p_slave -> do
      throwErrnoIfMinus1_ "openPty"
          (c_openpty p_master p_slave nullPtr nullPtr nullPtr)
      master <- peek p_master
      slave <- peek p_slave
      return (Fd master, Fd slave)

foreign import ccall unsafe "openpty"
  c_openpty :: Ptr CInt -> Ptr CInt -> CString -> Ptr CTermios -> Ptr a
            -> IO CInt

{-# LINE 605 "libraries/unix/./System/Posix/Terminal.hsc" #-}

-- -----------------------------------------------------------------------------
-- Local utility functions

-- Convert Haskell ControlCharacter to Int

cc2Word :: ControlCharacter -> Int
cc2Word EndOfFile = (4)
{-# LINE 613 "libraries/unix/./System/Posix/Terminal.hsc" #-}
cc2Word EndOfLine = (11)
{-# LINE 614 "libraries/unix/./System/Posix/Terminal.hsc" #-}
cc2Word Erase     = (2)
{-# LINE 615 "libraries/unix/./System/Posix/Terminal.hsc" #-}
cc2Word Interrupt = (0)
{-# LINE 616 "libraries/unix/./System/Posix/Terminal.hsc" #-}
cc2Word Kill      = (3)
{-# LINE 617 "libraries/unix/./System/Posix/Terminal.hsc" #-}
cc2Word Quit      = (1)
{-# LINE 618 "libraries/unix/./System/Posix/Terminal.hsc" #-}
cc2Word Suspend   = (10)
{-# LINE 619 "libraries/unix/./System/Posix/Terminal.hsc" #-}
cc2Word Start     = (8)
{-# LINE 620 "libraries/unix/./System/Posix/Terminal.hsc" #-}
cc2Word Stop      = (9)
{-# LINE 621 "libraries/unix/./System/Posix/Terminal.hsc" #-}

-- Convert Haskell BaudRate to unsigned integral type (Word)

baud2Word :: BaudRate -> CSpeed
baud2Word B0 = (0)
{-# LINE 626 "libraries/unix/./System/Posix/Terminal.hsc" #-}
baud2Word B50 = (1)
{-# LINE 627 "libraries/unix/./System/Posix/Terminal.hsc" #-}
baud2Word B75 = (2)
{-# LINE 628 "libraries/unix/./System/Posix/Terminal.hsc" #-}
baud2Word B110 = (3)
{-# LINE 629 "libraries/unix/./System/Posix/Terminal.hsc" #-}
baud2Word B134 = (4)
{-# LINE 630 "libraries/unix/./System/Posix/Terminal.hsc" #-}
baud2Word B150 = (5)
{-# LINE 631 "libraries/unix/./System/Posix/Terminal.hsc" #-}
baud2Word B200 = (6)
{-# LINE 632 "libraries/unix/./System/Posix/Terminal.hsc" #-}
baud2Word B300 = (7)
{-# LINE 633 "libraries/unix/./System/Posix/Terminal.hsc" #-}
baud2Word B600 = (8)
{-# LINE 634 "libraries/unix/./System/Posix/Terminal.hsc" #-}
baud2Word B1200 = (9)
{-# LINE 635 "libraries/unix/./System/Posix/Terminal.hsc" #-}
baud2Word B1800 = (10)
{-# LINE 636 "libraries/unix/./System/Posix/Terminal.hsc" #-}
baud2Word B2400 = (11)
{-# LINE 637 "libraries/unix/./System/Posix/Terminal.hsc" #-}
baud2Word B4800 = (12)
{-# LINE 638 "libraries/unix/./System/Posix/Terminal.hsc" #-}
baud2Word B9600 = (13)
{-# LINE 639 "libraries/unix/./System/Posix/Terminal.hsc" #-}
baud2Word B19200 = (14)
{-# LINE 640 "libraries/unix/./System/Posix/Terminal.hsc" #-}
baud2Word B38400 = (15)
{-# LINE 641 "libraries/unix/./System/Posix/Terminal.hsc" #-}
baud2Word B57600 = (4097)
{-# LINE 642 "libraries/unix/./System/Posix/Terminal.hsc" #-}
baud2Word B115200 = (4098)
{-# LINE 643 "libraries/unix/./System/Posix/Terminal.hsc" #-}

-- And convert a word back to a baud rate
-- We really need some cpp macros here.

word2Baud :: CSpeed -> BaudRate
word2Baud x =
    if x == (0) then B0
{-# LINE 650 "libraries/unix/./System/Posix/Terminal.hsc" #-}
    else if x == (1) then B50
{-# LINE 651 "libraries/unix/./System/Posix/Terminal.hsc" #-}
    else if x == (2) then B75
{-# LINE 652 "libraries/unix/./System/Posix/Terminal.hsc" #-}
    else if x == (3) then B110
{-# LINE 653 "libraries/unix/./System/Posix/Terminal.hsc" #-}
    else if x == (4) then B134
{-# LINE 654 "libraries/unix/./System/Posix/Terminal.hsc" #-}
    else if x == (5) then B150
{-# LINE 655 "libraries/unix/./System/Posix/Terminal.hsc" #-}
    else if x == (6) then B200
{-# LINE 656 "libraries/unix/./System/Posix/Terminal.hsc" #-}
    else if x == (7) then B300
{-# LINE 657 "libraries/unix/./System/Posix/Terminal.hsc" #-}
    else if x == (8) then B600
{-# LINE 658 "libraries/unix/./System/Posix/Terminal.hsc" #-}
    else if x == (9) then B1200
{-# LINE 659 "libraries/unix/./System/Posix/Terminal.hsc" #-}
    else if x == (10) then B1800
{-# LINE 660 "libraries/unix/./System/Posix/Terminal.hsc" #-}
    else if x == (11) then B2400
{-# LINE 661 "libraries/unix/./System/Posix/Terminal.hsc" #-}
    else if x == (12) then B4800
{-# LINE 662 "libraries/unix/./System/Posix/Terminal.hsc" #-}
    else if x == (13) then B9600
{-# LINE 663 "libraries/unix/./System/Posix/Terminal.hsc" #-}
    else if x == (14) then B19200
{-# LINE 664 "libraries/unix/./System/Posix/Terminal.hsc" #-}
    else if x == (15) then B38400
{-# LINE 665 "libraries/unix/./System/Posix/Terminal.hsc" #-}
    else if x == (4097) then B57600
{-# LINE 666 "libraries/unix/./System/Posix/Terminal.hsc" #-}
    else if x == (4098) then B115200
{-# LINE 667 "libraries/unix/./System/Posix/Terminal.hsc" #-}
    else error "unknown baud rate"

-- Clear termios i_flag

clearInputFlag :: CTcflag -> TerminalAttributes -> TerminalAttributes
clearInputFlag flag termios = unsafePerformIO $ do
  fp <- mallocForeignPtrBytes (60)
{-# LINE 674 "libraries/unix/./System/Posix/Terminal.hsc" #-}
  withForeignPtr fp $ \p1 -> do
    withTerminalAttributes termios $ \p2 -> do
      copyBytes p1 p2 (60) 
{-# LINE 677 "libraries/unix/./System/Posix/Terminal.hsc" #-}
      iflag <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) p2
{-# LINE 678 "libraries/unix/./System/Posix/Terminal.hsc" #-}
      ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) p1 (iflag .&. complement flag)
{-# LINE 679 "libraries/unix/./System/Posix/Terminal.hsc" #-}
  return $ makeTerminalAttributes fp

-- Set termios i_flag

setInputFlag :: CTcflag -> TerminalAttributes -> TerminalAttributes
setInputFlag flag termios = unsafePerformIO $ do
  fp <- mallocForeignPtrBytes (60)
{-# LINE 686 "libraries/unix/./System/Posix/Terminal.hsc" #-}
  withForeignPtr fp $ \p1 -> do
    withTerminalAttributes termios $ \p2 -> do
      copyBytes p1 p2 (60) 
{-# LINE 689 "libraries/unix/./System/Posix/Terminal.hsc" #-}
      iflag <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) p2
{-# LINE 690 "libraries/unix/./System/Posix/Terminal.hsc" #-}
      ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) p1 (iflag .|. flag)
{-# LINE 691 "libraries/unix/./System/Posix/Terminal.hsc" #-}
  return $ makeTerminalAttributes fp

-- Examine termios i_flag

testInputFlag :: CTcflag -> TerminalAttributes -> Bool
testInputFlag flag termios = unsafePerformIO $
  withTerminalAttributes termios $ \p ->  do
    iflag <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) p
{-# LINE 699 "libraries/unix/./System/Posix/Terminal.hsc" #-}
    return $! ((iflag .&. flag) /= 0)

-- Clear termios c_flag

clearControlFlag :: CTcflag -> TerminalAttributes -> TerminalAttributes
clearControlFlag flag termios = unsafePerformIO $ do
  fp <- mallocForeignPtrBytes (60)
{-# LINE 706 "libraries/unix/./System/Posix/Terminal.hsc" #-}
  withForeignPtr fp $ \p1 -> do
    withTerminalAttributes termios $ \p2 -> do
      copyBytes p1 p2 (60) 
{-# LINE 709 "libraries/unix/./System/Posix/Terminal.hsc" #-}
      cflag <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) p2
{-# LINE 710 "libraries/unix/./System/Posix/Terminal.hsc" #-}
      ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) p1 (cflag .&. complement flag)
{-# LINE 711 "libraries/unix/./System/Posix/Terminal.hsc" #-}
  return $ makeTerminalAttributes fp

-- Set termios c_flag

setControlFlag :: CTcflag -> TerminalAttributes -> TerminalAttributes
setControlFlag flag termios = unsafePerformIO $ do
  fp <- mallocForeignPtrBytes (60)
{-# LINE 718 "libraries/unix/./System/Posix/Terminal.hsc" #-}
  withForeignPtr fp $ \p1 -> do
    withTerminalAttributes termios $ \p2 -> do
      copyBytes p1 p2 (60) 
{-# LINE 721 "libraries/unix/./System/Posix/Terminal.hsc" #-}
      cflag <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) p2
{-# LINE 722 "libraries/unix/./System/Posix/Terminal.hsc" #-}
      ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) p1 (cflag .|. flag)
{-# LINE 723 "libraries/unix/./System/Posix/Terminal.hsc" #-}
  return $ makeTerminalAttributes fp

-- Examine termios c_flag

testControlFlag :: CTcflag -> TerminalAttributes -> Bool
testControlFlag flag termios = unsafePerformIO $
  withTerminalAttributes termios $ \p -> do
    cflag <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) p
{-# LINE 731 "libraries/unix/./System/Posix/Terminal.hsc" #-}
    return $! ((cflag .&. flag) /= 0)

-- Clear termios l_flag

clearLocalFlag :: CTcflag -> TerminalAttributes -> TerminalAttributes
clearLocalFlag flag termios = unsafePerformIO $ do
  fp <- mallocForeignPtrBytes (60)
{-# LINE 738 "libraries/unix/./System/Posix/Terminal.hsc" #-}
  withForeignPtr fp $ \p1 -> do
    withTerminalAttributes termios $ \p2 -> do
      copyBytes p1 p2 (60) 
{-# LINE 741 "libraries/unix/./System/Posix/Terminal.hsc" #-}
      lflag <- ((\hsc_ptr -> peekByteOff hsc_ptr 12)) p2
{-# LINE 742 "libraries/unix/./System/Posix/Terminal.hsc" #-}
      ((\hsc_ptr -> pokeByteOff hsc_ptr 12)) p1 (lflag .&. complement flag)
{-# LINE 743 "libraries/unix/./System/Posix/Terminal.hsc" #-}
  return $ makeTerminalAttributes fp

-- Set termios l_flag

setLocalFlag :: CTcflag -> TerminalAttributes -> TerminalAttributes
setLocalFlag flag termios = unsafePerformIO $ do
  fp <- mallocForeignPtrBytes (60)
{-# LINE 750 "libraries/unix/./System/Posix/Terminal.hsc" #-}
  withForeignPtr fp $ \p1 -> do
    withTerminalAttributes termios $ \p2 -> do
      copyBytes p1 p2 (60) 
{-# LINE 753 "libraries/unix/./System/Posix/Terminal.hsc" #-}
      lflag <- ((\hsc_ptr -> peekByteOff hsc_ptr 12)) p2
{-# LINE 754 "libraries/unix/./System/Posix/Terminal.hsc" #-}
      ((\hsc_ptr -> pokeByteOff hsc_ptr 12)) p1 (lflag .|. flag)
{-# LINE 755 "libraries/unix/./System/Posix/Terminal.hsc" #-}
  return $ makeTerminalAttributes fp

-- Examine termios l_flag

testLocalFlag :: CTcflag -> TerminalAttributes -> Bool
testLocalFlag flag termios = unsafePerformIO $
  withTerminalAttributes termios $ \p ->  do
    lflag <- ((\hsc_ptr -> peekByteOff hsc_ptr 12)) p
{-# LINE 763 "libraries/unix/./System/Posix/Terminal.hsc" #-}
    return $! ((lflag .&. flag) /= 0)

-- Clear termios o_flag

clearOutputFlag :: CTcflag -> TerminalAttributes -> TerminalAttributes
clearOutputFlag flag termios = unsafePerformIO $ do
  fp <- mallocForeignPtrBytes (60)
{-# LINE 770 "libraries/unix/./System/Posix/Terminal.hsc" #-}
  withForeignPtr fp $ \p1 -> do
    withTerminalAttributes termios $ \p2 -> do
      copyBytes p1 p2 (60) 
{-# LINE 773 "libraries/unix/./System/Posix/Terminal.hsc" #-}
      oflag <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) p2
{-# LINE 774 "libraries/unix/./System/Posix/Terminal.hsc" #-}
      ((\hsc_ptr -> pokeByteOff hsc_ptr 4)) p1 (oflag .&. complement flag)
{-# LINE 775 "libraries/unix/./System/Posix/Terminal.hsc" #-}
  return $ makeTerminalAttributes fp

-- Set termios o_flag

setOutputFlag :: CTcflag -> TerminalAttributes -> TerminalAttributes
setOutputFlag flag termios = unsafePerformIO $ do
  fp <- mallocForeignPtrBytes (60)
{-# LINE 782 "libraries/unix/./System/Posix/Terminal.hsc" #-}
  withForeignPtr fp $ \p1 -> do
    withTerminalAttributes termios $ \p2 -> do
      copyBytes p1 p2 (60) 
{-# LINE 785 "libraries/unix/./System/Posix/Terminal.hsc" #-}
      oflag <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) p2
{-# LINE 786 "libraries/unix/./System/Posix/Terminal.hsc" #-}
      ((\hsc_ptr -> pokeByteOff hsc_ptr 4)) p1 (oflag .|. flag)
{-# LINE 787 "libraries/unix/./System/Posix/Terminal.hsc" #-}
  return $ makeTerminalAttributes fp

-- Examine termios o_flag

testOutputFlag :: CTcflag -> TerminalAttributes -> Bool
testOutputFlag flag termios = unsafePerformIO $
  withTerminalAttributes termios $ \p -> do
    oflag <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) p
{-# LINE 795 "libraries/unix/./System/Posix/Terminal.hsc" #-}
    return $! ((oflag .&. flag) /= 0)

withNewTermios :: TerminalAttributes -> (Ptr CTermios -> IO a) 
  -> IO TerminalAttributes
withNewTermios termios action = do
  fp1 <- mallocForeignPtrBytes (60)
{-# LINE 801 "libraries/unix/./System/Posix/Terminal.hsc" #-}
  withForeignPtr fp1 $ \p1 -> do
   withTerminalAttributes termios $ \p2 -> do
    copyBytes p1 p2 (60)
{-# LINE 804 "libraries/unix/./System/Posix/Terminal.hsc" #-}
    _ <- action p1
    return ()
  return $ makeTerminalAttributes fp1
