{-# LINE 1 "libraries/unix/./System/Posix/User.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LINE 2 "libraries/unix/./System/Posix/User.hsc" #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.User
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX user\/group support
--
-----------------------------------------------------------------------------

module System.Posix.User (
    -- * User environment
    -- ** Querying the user environment
    getRealUserID,
    getRealGroupID,
    getEffectiveUserID,
    getEffectiveGroupID,
    getGroups,
    getLoginName,
    getEffectiveUserName,

    -- *** The group database
    GroupEntry(..),
    getGroupEntryForID,
    getGroupEntryForName,
    getAllGroupEntries,

    -- *** The user database
    UserEntry(..),
    getUserEntryForID,
    getUserEntryForName,
    getAllUserEntries,

    -- ** Modifying the user environment
    setUserID,
    setGroupID,
    setEffectiveUserID,
    setEffectiveGroupID,
    setGroups

  ) where


{-# LINE 49 "libraries/unix/./System/Posix/User.hsc" #-}

import System.Posix.Types
import Foreign hiding (unsafePerformIO)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.C
import System.Posix.Internals	( CGroup, CPasswd )


{-# LINE 57 "libraries/unix/./System/Posix/User.hsc" #-}
import Control.Concurrent.MVar  ( MVar, newMVar, withMVar )

{-# LINE 59 "libraries/unix/./System/Posix/User.hsc" #-}

{-# LINE 60 "libraries/unix/./System/Posix/User.hsc" #-}
import Control.Exception

{-# LINE 62 "libraries/unix/./System/Posix/User.hsc" #-}

{-# LINE 63 "libraries/unix/./System/Posix/User.hsc" #-}
import Control.Monad
import System.IO.Error

{-# LINE 66 "libraries/unix/./System/Posix/User.hsc" #-}

-- -----------------------------------------------------------------------------
-- user environemnt

-- | @getRealUserID@ calls @getuid@ to obtain the real @UserID@
--   associated with the current process.
getRealUserID :: IO UserID
getRealUserID = c_getuid

foreign import ccall unsafe "getuid"
  c_getuid :: IO CUid

-- | @getRealGroupID@ calls @getgid@ to obtain the real @GroupID@
--   associated with the current process.
getRealGroupID :: IO GroupID
getRealGroupID = c_getgid

foreign import ccall unsafe "getgid"
  c_getgid :: IO CGid

-- | @getEffectiveUserID@ calls @geteuid@ to obtain the effective
--   @UserID@ associated with the current process.
getEffectiveUserID :: IO UserID
getEffectiveUserID = c_geteuid

foreign import ccall unsafe "geteuid"
  c_geteuid :: IO CUid

-- | @getEffectiveGroupID@ calls @getegid@ to obtain the effective
--   @GroupID@ associated with the current process.
getEffectiveGroupID :: IO GroupID
getEffectiveGroupID = c_getegid

foreign import ccall unsafe "getegid"
  c_getegid :: IO CGid

-- | @getGroups@ calls @getgroups@ to obtain the list of
--   supplementary @GroupID@s associated with the current process.
getGroups :: IO [GroupID]
getGroups = do
    ngroups <- c_getgroups 0 nullPtr
    allocaArray (fromIntegral ngroups) $ \arr -> do
       throwErrnoIfMinus1_ "getGroups" (c_getgroups ngroups arr)
       groups <- peekArray (fromIntegral ngroups) arr
       return groups

foreign import ccall unsafe "getgroups"
  c_getgroups :: CInt -> Ptr CGid -> IO CInt


-- | @setGroups@ calls @setgroups@ to set the list of
--   supplementary @GroupID@s associated with the current process.
setGroups :: [GroupID] -> IO ()
setGroups groups = do
    withArrayLen groups $ \ ngroups arr ->
       throwErrnoIfMinus1_ "setGroups" (c_setgroups (fromIntegral ngroups) arr)

foreign import ccall unsafe "setgroups"
  c_setgroups :: CInt -> Ptr CGid -> IO CInt



-- | @getLoginName@ calls @getlogin@ to obtain the login name
--   associated with the current process.
getLoginName :: IO String
getLoginName =  do
    -- ToDo: use getlogin_r
    str <- throwErrnoIfNull "getLoginName" c_getlogin
    peekCString str

foreign import ccall unsafe "getlogin"
  c_getlogin :: IO CString

-- | @setUserID uid@ calls @setuid@ to set the real, effective, and
--   saved set-user-id associated with the current process to @uid@.
setUserID :: UserID -> IO ()
setUserID uid = throwErrnoIfMinus1_ "setUserID" (c_setuid uid)

foreign import ccall unsafe "setuid"
  c_setuid :: CUid -> IO CInt

-- | @setEffectiveUserID uid@ calls @seteuid@ to set the effective
--   user-id associated with the current process to @uid@. This
--   does not update the real user-id or set-user-id.
setEffectiveUserID :: UserID -> IO ()
setEffectiveUserID uid = throwErrnoIfMinus1_ "setEffectiveUserID" (c_seteuid uid)

foreign import ccall unsafe "seteuid"
  c_seteuid :: CUid -> IO CInt

-- | @setGroupID gid@ calls @setgid@ to set the real, effective, and
--   saved set-group-id associated with the current process to @gid@.
setGroupID :: GroupID -> IO ()
setGroupID gid = throwErrnoIfMinus1_ "setGroupID" (c_setgid gid)

foreign import ccall unsafe "setgid"
  c_setgid :: CGid -> IO CInt

-- | @setEffectiveGroupID uid@ calls @setegid@ to set the effective
--   group-id associated with the current process to @gid@. This
--   does not update the real group-id or set-group-id.
setEffectiveGroupID :: GroupID -> IO ()
setEffectiveGroupID gid =
  throwErrnoIfMinus1_ "setEffectiveGroupID" (c_setegid gid)


foreign import ccall unsafe "setegid"
  c_setegid :: CGid -> IO CInt

-- -----------------------------------------------------------------------------
-- User names

-- | @getEffectiveUserName@ gets the name
--   associated with the effective @UserID@ of the process.
getEffectiveUserName :: IO String
getEffectiveUserName = do
    euid <- getEffectiveUserID
    pw <- getUserEntryForID euid
    return (userName pw)

-- -----------------------------------------------------------------------------
-- The group database (grp.h)

data GroupEntry =
 GroupEntry {
  groupName    :: String,       -- ^ The name of this group (gr_name)
  groupPassword :: String,      -- ^ The password for this group (gr_passwd)
  groupID      :: GroupID,      -- ^ The unique numeric ID for this group (gr_gid)
  groupMembers :: [String]      -- ^ A list of zero or more usernames that are members (gr_mem)
 } deriving (Show, Read, Eq)

-- | @getGroupEntryForID gid@ calls @getgrgid@ to obtain
--   the @GroupEntry@ information associated with @GroupID@
--   @gid@.
getGroupEntryForID :: GroupID -> IO GroupEntry

{-# LINE 202 "libraries/unix/./System/Posix/User.hsc" #-}
getGroupEntryForID gid = do
  allocaBytes (32) $ \pgr ->
{-# LINE 204 "libraries/unix/./System/Posix/User.hsc" #-}
    alloca $ \ ppgr -> do
      throwErrorIfNonZero_ "getGroupEntryForID" $
	   doubleAllocWhile isERANGE grBufSize $ \s b ->
	     c_getgrgid_r gid pgr b (fromIntegral s) ppgr
      _ <- throwErrnoIfNull "getGroupEntryForID" $
	   peekElemOff ppgr 0
      unpackGroupEntry pgr


foreign import ccall unsafe "getgrgid_r"
  c_getgrgid_r :: CGid -> Ptr CGroup -> CString
		 -> CSize -> Ptr (Ptr CGroup) -> IO CInt

{-# LINE 219 "libraries/unix/./System/Posix/User.hsc" #-}

-- | @getGroupEntryForName name@ calls @getgrnam@ to obtain
--   the @GroupEntry@ information associated with the group called
--   @name@.
getGroupEntryForName :: String -> IO GroupEntry

{-# LINE 225 "libraries/unix/./System/Posix/User.hsc" #-}
getGroupEntryForName name = do
  allocaBytes (32) $ \pgr ->
{-# LINE 227 "libraries/unix/./System/Posix/User.hsc" #-}
    alloca $ \ ppgr ->
      withCString name $ \ pstr -> do
	throwErrorIfNonZero_ "getGroupEntryForName" $
	  doubleAllocWhile isERANGE grBufSize $ \s b ->
	    c_getgrnam_r pstr pgr b (fromIntegral s) ppgr
	r <- peekElemOff ppgr 0
	when (r == nullPtr) $
	  ioError $ flip ioeSetErrorString "no group name"
		  $ mkIOError doesNotExistErrorType
			      "getGroupEntryForName"
			      Nothing
			      (Just name)
	unpackGroupEntry pgr

foreign import ccall unsafe "getgrnam_r"
  c_getgrnam_r :: CString -> Ptr CGroup -> CString
		 -> CSize -> Ptr (Ptr CGroup) -> IO CInt

{-# LINE 247 "libraries/unix/./System/Posix/User.hsc" #-}

-- | @getAllGroupEntries@ returns all group entries on the system by
--   repeatedly calling @getgrent@

--
-- getAllGroupEntries may fail with isDoesNotExistError on Linux due to
-- this bug in glibc:
--   http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=466647
--
getAllGroupEntries :: IO [GroupEntry]

{-# LINE 258 "libraries/unix/./System/Posix/User.hsc" #-}
getAllGroupEntries =
    withMVar lock $ \_ -> bracket_ c_setgrent c_endgrent $ worker []
    where worker accum =
              do resetErrno
                 ppw <- throwErrnoIfNullAndError "getAllGroupEntries" $ 
                        c_getgrent
                 if ppw == nullPtr
                     then return (reverse accum)
                     else do thisentry <- unpackGroupEntry ppw
                             worker (thisentry : accum)

foreign import ccall unsafe "getgrent"
  c_getgrent :: IO (Ptr CGroup)
foreign import ccall unsafe "setgrent"
  c_setgrent :: IO ()
foreign import ccall unsafe "endgrent"
  c_endgrent :: IO ()

{-# LINE 278 "libraries/unix/./System/Posix/User.hsc" #-}


{-# LINE 280 "libraries/unix/./System/Posix/User.hsc" #-}
grBufSize :: Int

{-# LINE 282 "libraries/unix/./System/Posix/User.hsc" #-}
grBufSize = sysconfWithDefault 1024 (69)
{-# LINE 283 "libraries/unix/./System/Posix/User.hsc" #-}

{-# LINE 286 "libraries/unix/./System/Posix/User.hsc" #-}

{-# LINE 287 "libraries/unix/./System/Posix/User.hsc" #-}

unpackGroupEntry :: Ptr CGroup -> IO GroupEntry
unpackGroupEntry ptr = do
   name    <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr >>= peekCString
{-# LINE 291 "libraries/unix/./System/Posix/User.hsc" #-}
   passwd  <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr >>= peekCString
{-# LINE 292 "libraries/unix/./System/Posix/User.hsc" #-}
   gid     <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) ptr
{-# LINE 293 "libraries/unix/./System/Posix/User.hsc" #-}
   mem     <- ((\hsc_ptr -> peekByteOff hsc_ptr 24)) ptr
{-# LINE 294 "libraries/unix/./System/Posix/User.hsc" #-}
   members <- peekArray0 nullPtr mem >>= mapM peekCString
   return (GroupEntry name passwd gid members)

-- -----------------------------------------------------------------------------
-- The user database (pwd.h)

data UserEntry =
 UserEntry {
   userName      :: String,     -- ^ Textual name of this user (pw_name)
   userPassword  :: String,     -- ^ Password -- may be empty or fake if shadow is in use (pw_passwd)
   userID        :: UserID,     -- ^ Numeric ID for this user (pw_uid)
   userGroupID   :: GroupID,    -- ^ Primary group ID (pw_gid)
   userGecos     :: String,     -- ^ Usually the real name for the user (pw_gecos)
   homeDirectory :: String,     -- ^ Home directory (pw_dir)
   userShell     :: String      -- ^ Default shell (pw_shell)
 } deriving (Show, Read, Eq)

--
-- getpwuid and getpwnam leave results in a static object. Subsequent
-- calls modify the same object, which isn't threadsafe. We attempt to
-- mitigate this issue, on platforms that don't provide the safe _r versions
--
-- Also, getpwent/setpwent require a global lock since they maintain
-- an internal file position pointer.

{-# LINE 319 "libraries/unix/./System/Posix/User.hsc" #-}
lock :: MVar ()
lock = unsafePerformIO $ newMVar ()
{-# NOINLINE lock #-}

{-# LINE 323 "libraries/unix/./System/Posix/User.hsc" #-}

-- | @getUserEntryForID gid@ calls @getpwuid@ to obtain
--   the @UserEntry@ information associated with @UserID@
--   @uid@.
getUserEntryForID :: UserID -> IO UserEntry

{-# LINE 329 "libraries/unix/./System/Posix/User.hsc" #-}
getUserEntryForID uid = do
  allocaBytes (48) $ \ppw ->
{-# LINE 331 "libraries/unix/./System/Posix/User.hsc" #-}
    alloca $ \ pppw -> do
      throwErrorIfNonZero_ "getUserEntryForID" $
	   doubleAllocWhile isERANGE pwBufSize $ \s b ->
	     c_getpwuid_r uid ppw b (fromIntegral s) pppw
      _ <- throwErrnoIfNull "getUserEntryForID" $
	   peekElemOff pppw 0
      unpackUserEntry ppw

foreign import ccall unsafe "__hsunix_getpwuid_r"
  c_getpwuid_r :: CUid -> Ptr CPasswd -> 
			CString -> CSize -> Ptr (Ptr CPasswd) -> IO CInt

{-# LINE 353 "libraries/unix/./System/Posix/User.hsc" #-}

-- | @getUserEntryForName name@ calls @getpwnam@ to obtain
--   the @UserEntry@ information associated with the user login
--   @name@.
getUserEntryForName :: String -> IO UserEntry

{-# LINE 359 "libraries/unix/./System/Posix/User.hsc" #-}
getUserEntryForName name = do
  allocaBytes (48) $ \ppw ->
{-# LINE 361 "libraries/unix/./System/Posix/User.hsc" #-}
    alloca $ \ pppw ->
      withCString name $ \ pstr -> do
	throwErrorIfNonZero_ "getUserEntryForName" $
	  doubleAllocWhile isERANGE pwBufSize $ \s b ->
	    c_getpwnam_r pstr ppw b (fromIntegral s) pppw
	r <- peekElemOff pppw 0
	when (r == nullPtr) $
	  ioError $ flip ioeSetErrorString "no user name"
		  $ mkIOError doesNotExistErrorType
			      "getUserEntryForName"
			      Nothing
			      (Just name)
	unpackUserEntry ppw

foreign import ccall unsafe "__hsunix_getpwnam_r"
  c_getpwnam_r :: CString -> Ptr CPasswd
               -> CString -> CSize -> Ptr (Ptr CPasswd) -> IO CInt

{-# LINE 390 "libraries/unix/./System/Posix/User.hsc" #-}

-- | @getAllUserEntries@ returns all user entries on the system by 
--   repeatedly calling @getpwent@
getAllUserEntries :: IO [UserEntry]

{-# LINE 395 "libraries/unix/./System/Posix/User.hsc" #-}
getAllUserEntries = 
    withMVar lock $ \_ -> bracket_ c_setpwent c_endpwent $ worker []
    where worker accum = 
              do resetErrno
                 ppw <- throwErrnoIfNullAndError "getAllUserEntries" $ 
                        c_getpwent
                 if ppw == nullPtr
                     then return (reverse accum)
                     else do thisentry <- unpackUserEntry ppw
                             worker (thisentry : accum)

foreign import ccall unsafe "__hsunix_getpwent"
  c_getpwent :: IO (Ptr CPasswd)
foreign import ccall unsafe "setpwent"
  c_setpwent :: IO ()
foreign import ccall unsafe "endpwent"
  c_endpwent :: IO ()

{-# LINE 415 "libraries/unix/./System/Posix/User.hsc" #-}


{-# LINE 417 "libraries/unix/./System/Posix/User.hsc" #-}
pwBufSize :: Int

{-# LINE 419 "libraries/unix/./System/Posix/User.hsc" #-}
pwBufSize = sysconfWithDefault 1024 (70)
{-# LINE 420 "libraries/unix/./System/Posix/User.hsc" #-}

{-# LINE 423 "libraries/unix/./System/Posix/User.hsc" #-}

{-# LINE 424 "libraries/unix/./System/Posix/User.hsc" #-}


{-# LINE 426 "libraries/unix/./System/Posix/User.hsc" #-}
foreign import ccall unsafe "sysconf"
  c_sysconf :: CInt -> IO CLong

-- We need a default value since sysconf can fail and return -1
-- even when the parameter name is defined in unistd.h.
-- One example of this is _SC_GETPW_R_SIZE_MAX under 
-- Mac OS X 10.4.9 on i386.
sysconfWithDefault :: Int -> CInt -> Int
sysconfWithDefault def sc = 
    unsafePerformIO $ do v <- fmap fromIntegral $ c_sysconf sc
                         return $ if v == (-1) then def else v

{-# LINE 438 "libraries/unix/./System/Posix/User.hsc" #-}

isERANGE :: Integral a => a -> Bool
isERANGE = (== eRANGE) . Errno . fromIntegral

doubleAllocWhile :: (a -> Bool) -> Int -> (Int -> Ptr b -> IO a) -> IO a
doubleAllocWhile p s m = do
  r <- allocaBytes s (m s)
  if p r then doubleAllocWhile p (2 * s) m else return r

unpackUserEntry :: Ptr CPasswd -> IO UserEntry
unpackUserEntry ptr = do
   name   <- ((\hsc_ptr -> peekByteOff hsc_ptr 0))   ptr >>= peekCString
{-# LINE 450 "libraries/unix/./System/Posix/User.hsc" #-}
   passwd <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr >>= peekCString
{-# LINE 451 "libraries/unix/./System/Posix/User.hsc" #-}
   uid    <- ((\hsc_ptr -> peekByteOff hsc_ptr 16))    ptr
{-# LINE 452 "libraries/unix/./System/Posix/User.hsc" #-}
   gid    <- ((\hsc_ptr -> peekByteOff hsc_ptr 20))    ptr
{-# LINE 453 "libraries/unix/./System/Posix/User.hsc" #-}
   gecos  <- ((\hsc_ptr -> peekByteOff hsc_ptr 24))  ptr >>= peekCString
{-# LINE 454 "libraries/unix/./System/Posix/User.hsc" #-}
   dir    <- ((\hsc_ptr -> peekByteOff hsc_ptr 32))    ptr >>= peekCString
{-# LINE 455 "libraries/unix/./System/Posix/User.hsc" #-}
   shell  <- ((\hsc_ptr -> peekByteOff hsc_ptr 40))  ptr >>= peekCString
{-# LINE 456 "libraries/unix/./System/Posix/User.hsc" #-}
   return (UserEntry name passwd uid gid gecos dir shell)

-- Used when calling re-entrant system calls that signal their 'errno' 
-- directly through the return value.
throwErrorIfNonZero_ :: String -> IO CInt -> IO ()
throwErrorIfNonZero_ loc act = do
    rc <- act
    if (rc == 0) 
     then return ()
     else ioError (errnoToIOError loc (Errno (fromIntegral rc)) Nothing Nothing)

-- Used when a function returns NULL to indicate either an error or
-- EOF, depending on whether the global errno is nonzero.
throwErrnoIfNullAndError :: String -> IO (Ptr a) -> IO (Ptr a)
throwErrnoIfNullAndError loc act = do
    rc <- act
    errno <- getErrno
    if rc == nullPtr && errno /= eOK
       then throwErrno loc
       else return rc
