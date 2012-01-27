module System.Console.Haskeline.Backend.Win32(
                win32Term,
                win32TermStdin,
                fileRunTerm
                )where


import System.IO
import Foreign
import Foreign.C
import System.Win32 hiding (multiByteToWideChar)
import Graphics.Win32.Misc(getStdHandle, sTD_OUTPUT_HANDLE)
import Data.List(intercalate)
import Control.Concurrent hiding (throwTo)
import Data.Char(isPrint)
import Data.Maybe(mapMaybe)
import Control.Monad

import System.Console.Haskeline.Key
import System.Console.Haskeline.Monads
import System.Console.Haskeline.LineState
import System.Console.Haskeline.Term as Term

import Data.ByteString.Internal (createAndTrim)
import qualified Data.ByteString as B

#include "win_console.h"

foreign import stdcall "windows.h ReadConsoleInputW" c_ReadConsoleInput
    :: HANDLE -> Ptr () -> DWORD -> Ptr DWORD -> IO Bool
    
foreign import stdcall "windows.h WaitForSingleObject" c_WaitForSingleObject
    :: HANDLE -> DWORD -> IO DWORD

foreign import stdcall "windows.h GetNumberOfConsoleInputEvents"
    c_GetNumberOfConsoleInputEvents :: HANDLE -> Ptr DWORD -> IO Bool

getNumberOfEvents :: HANDLE -> IO Int
getNumberOfEvents h = alloca $ \numEventsPtr -> do
    failIfFalse_ "GetNumberOfConsoleInputEvents"
        $ c_GetNumberOfConsoleInputEvents h numEventsPtr
    fmap fromEnum $ peek numEventsPtr

getEvent :: HANDLE -> Chan Event -> IO Event
getEvent h = keyEventLoop (eventReader h)

eventReader :: HANDLE -> IO [Event]
eventReader h = do
    let waitTime = 500 -- milliseconds
    ret <- c_WaitForSingleObject h waitTime
    yield -- otherwise, the above foreign call causes the loop to never 
          -- respond to the killThread
    if ret /= (#const WAIT_OBJECT_0)
        then eventReader h
        else do
            es <- readEvents h
            return $ mapMaybe processEvent es

consoleHandles :: MaybeT IO Handles
consoleHandles = do
    h_in <- open "CONIN$"
    h_out <- open "CONOUT$"
    return Handles { hIn = h_in, hOut = h_out }
  where
   open file = handle (\(_::IOException) -> mzero) $ liftIO
                $ createFile file (gENERIC_READ .|. gENERIC_WRITE)
                        (fILE_SHARE_READ .|. fILE_SHARE_WRITE) Nothing
                        oPEN_EXISTING 0 Nothing

                       
processEvent :: InputEvent -> Maybe Event
processEvent KeyEvent {keyDown = True, unicodeChar = c, virtualKeyCode = vc,
                    controlKeyState = cstate}
    = fmap (\e -> KeyInput [Key modifier' e]) $ keyFromCode vc `mplus` simpleKeyChar
  where
    simpleKeyChar = guard (c /= '\NUL') >> return (KeyChar c)
    testMod ck = (cstate .&. ck) /= 0
    modifier' = if hasMeta modifier && hasControl modifier
                    then noModifier {hasShift = hasShift modifier}
                    else modifier
    modifier = Modifier {hasMeta = testMod ((#const RIGHT_ALT_PRESSED) 
                                        .|. (#const LEFT_ALT_PRESSED))
                        ,hasControl = testMod ((#const RIGHT_CTRL_PRESSED) 
                                        .|. (#const LEFT_CTRL_PRESSED))
                                    && not (c > '\NUL' && c <= '\031')
                        ,hasShift = testMod (#const SHIFT_PRESSED)
                                    && not (isPrint c)
                        }

processEvent WindowEvent = Just WindowResize
processEvent _ = Nothing

keyFromCode :: WORD -> Maybe BaseKey
keyFromCode (#const VK_BACK) = Just Backspace
keyFromCode (#const VK_LEFT) = Just LeftKey
keyFromCode (#const VK_RIGHT) = Just RightKey
keyFromCode (#const VK_UP) = Just UpKey
keyFromCode (#const VK_DOWN) = Just DownKey
keyFromCode (#const VK_DELETE) = Just Delete
keyFromCode (#const VK_HOME) = Just Home
keyFromCode (#const VK_END) = Just End
keyFromCode (#const VK_PRIOR) = Just PageUp
keyFromCode (#const VK_NEXT) = Just PageDown
-- The Windows console will return '\r' when return is pressed.
keyFromCode (#const VK_RETURN) = Just (KeyChar '\n')
-- TODO: KillLine?
-- TODO: function keys.
keyFromCode _ = Nothing
    
data InputEvent = KeyEvent {keyDown :: BOOL,
                          repeatCount :: WORD,
                          virtualKeyCode :: WORD,
                          virtualScanCode :: WORD,
                          unicodeChar :: Char,
                          controlKeyState :: DWORD}
            -- TODO: WINDOW_BUFFER_SIZE_RECORD
            -- I cant figure out how the user generates them.
           | WindowEvent
           | OtherEvent
                        deriving Show

peekEvent :: Ptr () -> IO InputEvent
peekEvent pRecord = do
    eventType :: WORD <- (#peek INPUT_RECORD, EventType) pRecord
    let eventPtr = (#ptr INPUT_RECORD, Event) pRecord
    case eventType of
        (#const KEY_EVENT) -> getKeyEvent eventPtr
        (#const WINDOW_BUFFER_SIZE_EVENT) -> return WindowEvent
        _ -> return OtherEvent

readEvents :: HANDLE -> IO [InputEvent]
readEvents h = do
    n <- getNumberOfEvents h
    alloca $ \numEventsPtr -> 
        allocaBytes (n * #size INPUT_RECORD) $ \pRecord -> do
            failIfFalse_ "ReadConsoleInput" 
                $ c_ReadConsoleInput h pRecord (toEnum n) numEventsPtr
            numRead <- fmap fromEnum $ peek numEventsPtr
            forM [0..toEnum numRead-1] $ \i -> peekEvent
                $ pRecord `plusPtr` (i * #size INPUT_RECORD)

getKeyEvent :: Ptr () -> IO InputEvent
getKeyEvent p = do
    kDown' <- (#peek KEY_EVENT_RECORD, bKeyDown) p
    repeat' <- (#peek KEY_EVENT_RECORD, wRepeatCount) p
    keyCode <- (#peek KEY_EVENT_RECORD, wVirtualKeyCode) p
    scanCode <- (#peek KEY_EVENT_RECORD, wVirtualScanCode) p
    char :: CWchar <- (#peek KEY_EVENT_RECORD, uChar) p
    state <- (#peek KEY_EVENT_RECORD, dwControlKeyState) p
    return KeyEvent {keyDown = kDown',
                            repeatCount = repeat',
                            virtualKeyCode = keyCode,
                            virtualScanCode = scanCode,
                            unicodeChar = toEnum (fromEnum char),
                            controlKeyState = state}

data Coord = Coord {coordX, coordY :: Int}
                deriving Show
                
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
instance Storable Coord where
    sizeOf _ = (#size COORD)
    alignment _ = (#alignment COORD)
    peek p = do
        x :: CShort <- (#peek COORD, X) p
        y :: CShort <- (#peek COORD, Y) p
        return Coord {coordX = fromEnum x, coordY = fromEnum y}
    poke p c = do
        (#poke COORD, X) p (toEnum (coordX c) :: CShort)
        (#poke COORD, Y) p (toEnum (coordY c) :: CShort)
                
                            
foreign import ccall "SetPosition"
    c_SetPosition :: HANDLE -> Ptr Coord -> IO Bool
    
setPosition :: HANDLE -> Coord -> IO ()
setPosition h c = with c $ failIfFalse_ "SetConsoleCursorPosition" 
                    . c_SetPosition h
                    
foreign import stdcall "windows.h GetConsoleScreenBufferInfo"
    c_GetScreenBufferInfo :: HANDLE -> Ptr () -> IO Bool
    
getPosition :: HANDLE -> IO Coord
getPosition = withScreenBufferInfo $ 
    (#peek CONSOLE_SCREEN_BUFFER_INFO, dwCursorPosition)

withScreenBufferInfo :: (Ptr () -> IO a) -> HANDLE -> IO a
withScreenBufferInfo f h = allocaBytes (#size CONSOLE_SCREEN_BUFFER_INFO)
                                $ \infoPtr -> do
        failIfFalse_ "GetConsoleScreenBufferInfo"
            $ c_GetScreenBufferInfo h infoPtr
        f infoPtr

getBufferSize :: HANDLE -> IO Layout
getBufferSize = withScreenBufferInfo $ \p -> do
    c <- (#peek CONSOLE_SCREEN_BUFFER_INFO, dwSize) p
    return Layout {width = coordX c, height = coordY c}

foreign import stdcall "windows.h WriteConsoleW" c_WriteConsoleW
    :: HANDLE -> Ptr TCHAR -> DWORD -> Ptr DWORD -> Ptr () -> IO Bool

writeConsole :: HANDLE -> String -> IO ()
-- For some reason, Wine returns False when WriteConsoleW is called on an empty
-- string.  Easiest fix: just don't call that function.
writeConsole _ "" = return ()
writeConsole h str = withArray tstr $ \t_arr -> alloca $ \numWritten -> do
    failIfFalse_ "WriteConsole" 
        $ c_WriteConsoleW h t_arr (toEnum $ length str) numWritten nullPtr
  where
    tstr = map (toEnum . fromEnum) str

foreign import stdcall "windows.h MessageBeep" c_messageBeep :: UINT -> IO Bool

messageBeep :: IO ()
messageBeep = c_messageBeep (-1) >> return ()-- intentionally ignore failures.


----------
-- Console mode
foreign import stdcall "windows.h GetConsoleMode" c_GetConsoleMode
    :: HANDLE -> Ptr DWORD -> IO Bool

foreign import stdcall "windows.h SetConsoleMode" c_SetConsoleMode
    :: HANDLE -> DWORD -> IO Bool

withWindowMode :: MonadException m => Handles -> m a -> m a
withWindowMode hs f = do
    let h = hIn hs
    bracket (getConsoleMode h) (setConsoleMode h)
            $ \m -> setConsoleMode h (m .|. (#const ENABLE_WINDOW_INPUT)) >> f
  where
    getConsoleMode h = liftIO $ alloca $ \p -> do
            failIfFalse_ "GetConsoleMode" $ c_GetConsoleMode h p
            peek p
    setConsoleMode h m = liftIO $ failIfFalse_ "SetConsoleMode" $ c_SetConsoleMode h m

----------------------------
-- Drawing

data Handles = Handles { hIn, hOut :: HANDLE }

closeHandles :: Handles -> IO ()
closeHandles hs = closeHandle (hIn hs) >> closeHandle (hOut hs)

newtype Draw m a = Draw {runDraw :: ReaderT Handles m a}
    deriving (Monad,MonadIO,MonadException, MonadReader Handles)

type DrawM a = (MonadIO m, MonadReader Layout m) => Draw m ()

instance MonadTrans Draw where
    lift = Draw . lift

getPos :: MonadIO m => Draw m Coord
getPos = asks hOut >>= liftIO . getPosition
    
setPos :: MonadIO m => Coord -> Draw m ()
setPos c = do
    h <- asks hOut
    liftIO (setPosition h c)

printText :: MonadIO m => String -> Draw m ()
printText txt = do
    h <- asks hOut
    liftIO (writeConsole h txt)
    
printAfter :: String -> DrawM ()
printAfter str = do
    printText str
    movePos $ negate $ length str
    
drawLineDiffWin :: LineChars -> LineChars -> DrawM ()
drawLineDiffWin (xs1,ys1) (xs2,ys2) = case matchInit xs1 xs2 of
    ([],[])     | ys1 == ys2            -> return ()
    (xs1',[])   | xs1' ++ ys1 == ys2    -> movePos $ negate $ length xs1'
    ([],xs2')   | ys1 == xs2' ++ ys2    -> movePos $ length xs2'
    (xs1',xs2')                         -> do
        movePos (negate $ length xs1')
        let m = length xs1' + length ys1 - (length xs2' + length ys2)
        let deadText = replicate m ' '
        printText (graphemesToString xs2')
        printAfter (graphemesToString ys2 ++ deadText)

movePos :: Int -> DrawM ()
movePos n = do
    Coord {coordX = x, coordY = y} <- getPos
    w <- asks width
    let (h,x') = divMod (x+n) w
    setPos Coord {coordX = x', coordY = y+h}

crlf :: String
crlf = "\r\n"

instance (MonadException m, MonadReader Layout m) => Term (Draw m) where
    drawLineDiff (xs1,ys1) (xs2,ys2) = let
        fixEsc = filter ((/= '\ESC') . baseChar)
        in drawLineDiffWin (fixEsc xs1, fixEsc ys1) (fixEsc xs2, fixEsc ys2)
    -- TODO now that we capture resize events.
    -- first, looks like the cursor stays on the same line but jumps
    -- to the beginning if cut off.
    reposition _ _ = return ()

    printLines [] = return ()
    printLines ls = printText $ intercalate crlf ls ++ crlf
    
    clearLayout = do
        lay <- ask
        setPos (Coord 0 0)
        printText (replicate (width lay * height lay) ' ')
        setPos (Coord 0 0)
    
    moveToNextLine s = do
        movePos (lengthToEnd s)
        printText "\r\n" -- make the console take care of creating a new line
    
    ringBell True = liftIO messageBeep
    ringBell False = return () -- TODO

win32TermStdin :: MaybeT IO RunTerm
win32TermStdin = do
    liftIO (hIsTerminalDevice stdin) >>= guard
    win32Term

win32Term :: MaybeT IO RunTerm
win32Term = do
    hs <- consoleHandles
    ch <- liftIO newChan
    fileRT <- liftIO $ fileRunTerm stdin
    return fileRT {
                            termOps = Left TermOps {
                                getLayout = getBufferSize (hOut hs)
                                , withGetEvent = withWindowMode hs
                                                    . win32WithEvent hs ch
                                , runTerm = \(RunTermType f) ->
                                        runReaderT' hs $ runDraw f
                                },
                            closeTerm = closeHandles hs
                        }

win32WithEvent :: MonadException m => Handles -> Chan Event
                                        -> (m Event -> m a) -> m a
win32WithEvent h eventChan f = f $ liftIO $ getEvent (hIn h) eventChan

-- stdin is not a terminal, but we still need to check the right way to output unicode to stdout.
fileRunTerm :: Handle -> IO RunTerm
fileRunTerm h_in = do
    putter <- putOut
    cp <- getCodePage
    return RunTerm {
                    closeTerm = return (),
                    putStrOut = putter,
                    encodeForTerm = unicodeToCodePage cp,
                    decodeForTerm = codePageToUnicode cp,
                    wrapInterrupt = withCtrlCHandler,
                    termOps = Right FileOps {
                                inputHandle = h_in,
                                getLocaleChar = getMultiByteChar cp h_in,
                                maybeReadNewline = hMaybeReadNewline h_in,
                                getLocaleLine = Term.hGetLine h_in
                                            >>= liftIO . codePageToUnicode cp
                            }

                    }

-- On Windows, Unicode written to the console must be written with the WriteConsole API call.
-- And to make the API cross-platform consistent, Unicode to a file should be UTF-8.
putOut :: IO (String -> IO ())
putOut = do
    outIsTerm <- hIsTerminalDevice stdout
    if outIsTerm
        then do
            h <- getStdHandle sTD_OUTPUT_HANDLE
            return (writeConsole h)
        else do
            cp <- getCodePage
            return $ \str -> unicodeToCodePage cp str >>= B.putStr >> hFlush stdout



type Handler = DWORD -> IO BOOL

foreign import ccall "wrapper" wrapHandler :: Handler -> IO (FunPtr Handler)

foreign import stdcall "windows.h SetConsoleCtrlHandler" c_SetConsoleCtrlHandler
    :: FunPtr Handler -> BOOL -> IO BOOL

-- sets the tv to True when ctrl-c is pressed.
withCtrlCHandler :: MonadException m => m a -> m a
withCtrlCHandler f = bracket (liftIO $ do
                                    tid <- myThreadId
                                    fp <- wrapHandler (handler tid)
                                -- don't fail if we can't set the ctrl-c handler
                                -- for example, we might not be attached to a console?
                                    _ <- c_SetConsoleCtrlHandler fp True
                                    return fp)
                                (\fp -> liftIO $ c_SetConsoleCtrlHandler fp False)
                                (const f)
  where
    handler tid (#const CTRL_C_EVENT) = do
        throwTo tid Interrupt
        return True
    handler _ _ = return False


------------------------
-- Multi-byte conversion

foreign import stdcall "WideCharToMultiByte" wideCharToMultiByte
        :: CodePage -> DWORD -> LPCWSTR -> CInt -> LPCSTR -> CInt
                -> LPCSTR -> LPBOOL -> IO CInt

unicodeToCodePage :: CodePage -> String -> IO B.ByteString
unicodeToCodePage cp wideStr = withCWStringLen wideStr $ \(wideBuff, wideLen) -> do
    -- first, ask for the length without filling the buffer.
    outSize <- wideCharToMultiByte cp 0 wideBuff (toEnum wideLen)
                    nullPtr 0 nullPtr nullPtr
    -- then, actually perform the encoding.
    createAndTrim (fromEnum outSize) $ \outBuff -> 
        fmap fromEnum $ wideCharToMultiByte cp 0 wideBuff (toEnum wideLen)
                    (castPtr outBuff) outSize nullPtr nullPtr

foreign import stdcall "MultiByteToWideChar" multiByteToWideChar
        :: CodePage -> DWORD -> LPCSTR -> CInt -> LPWSTR -> CInt -> IO CInt

codePageToUnicode :: CodePage -> B.ByteString -> IO String
codePageToUnicode cp bs = B.useAsCStringLen bs $ \(inBuff, inLen) -> do
    -- first ask for the size without filling the buffer.
    outSize <- multiByteToWideChar cp 0 inBuff (toEnum inLen) nullPtr 0
    -- then, actually perform the decoding.
    allocaArray0 (fromEnum outSize) $ \outBuff -> do
    outSize' <- multiByteToWideChar cp 0 inBuff (toEnum inLen) outBuff outSize
    peekCWStringLen (outBuff, fromEnum outSize')
                

getCodePage :: IO CodePage
getCodePage = do
    conCP <- getConsoleCP
    if conCP > 0
        then return conCP
        else getACP

foreign import stdcall "IsDBCSLeadByteEx" c_IsDBCSLeadByteEx
        :: CodePage -> BYTE -> BOOL

getMultiByteChar :: CodePage -> Handle -> MaybeT IO Char
getMultiByteChar cp h = hWithBinaryMode h loop
  where
    loop = do
        b1 <- hGetByte h
        bs <- if c_IsDBCSLeadByteEx cp b1
                then hGetByte h >>= \b2 -> return [b1,b2]
                else return [b1]
        cs <- liftIO $ codePageToUnicode cp (B.pack bs)
        case cs of
            [] -> loop
            (c:_) -> return c
