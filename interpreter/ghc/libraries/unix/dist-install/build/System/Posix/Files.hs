{-# LINE 1 "libraries/unix/./System/Posix/Files.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LINE 2 "libraries/unix/./System/Posix/Files.hsc" #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Files
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- Functions defined by the POSIX standards for manipulating and querying the
-- file system. Names of underlying POSIX functions are indicated whenever
-- possible. A more complete documentation of the POSIX functions together
-- with a more detailed description of different error conditions are usually
-- available in the system's manual pages or from
-- <http://www.unix.org/version3/online.html> (free registration required).
--
-- When a function that calls an underlying POSIX function fails, the errno
-- code is converted to an 'IOError' using 'Foreign.C.Error.errnoToIOError'.
-- For a list of which errno codes may be generated, consult the POSIX
-- documentation for the underlying function.
--
-----------------------------------------------------------------------------

module System.Posix.Files (
    -- * File modes
    -- FileMode exported by System.Posix.Types
    unionFileModes, intersectFileModes,
    nullFileMode,
    ownerReadMode, ownerWriteMode, ownerExecuteMode, ownerModes,
    groupReadMode, groupWriteMode, groupExecuteMode, groupModes,
    otherReadMode, otherWriteMode, otherExecuteMode, otherModes,
    setUserIDMode, setGroupIDMode,
    stdFileMode,   accessModes,
    fileTypeModes,
    blockSpecialMode, characterSpecialMode, namedPipeMode, regularFileMode,
    directoryMode, symbolicLinkMode, socketMode,

    -- ** Setting file modes
    setFileMode, setFdMode, setFileCreationMask,

    -- ** Checking file existence and permissions
    fileAccess, fileExist,

    -- * File status
    FileStatus,
    -- ** Obtaining file status
    getFileStatus, getFdStatus, getSymbolicLinkStatus,
    -- ** Querying file status
    deviceID, fileID, fileMode, linkCount, fileOwner, fileGroup,
    specialDeviceID, fileSize, accessTime, modificationTime,
    statusChangeTime,
    isBlockDevice, isCharacterDevice, isNamedPipe, isRegularFile,
    isDirectory, isSymbolicLink, isSocket,

    -- * Creation
    createNamedPipe, 
    createDevice,

    -- * Hard links
    createLink, removeLink,

    -- * Symbolic links
    createSymbolicLink, readSymbolicLink,

    -- * Renaming files
    rename,

    -- * Changing file ownership
    setOwnerAndGroup,  setFdOwnerAndGroup,

{-# LINE 74 "libraries/unix/./System/Posix/Files.hsc" #-}
    setSymbolicLinkOwnerAndGroup,

{-# LINE 76 "libraries/unix/./System/Posix/Files.hsc" #-}

    -- * Changing file timestamps
    setFileTimes, touchFile,

    -- * Setting file sizes
    setFileSize, setFdSize,

    -- * Find system-specific limits for a file
    PathVar(..), getPathVar, getFdPathVar,
  ) where


{-# LINE 88 "libraries/unix/./System/Posix/Files.hsc" #-}

import System.Posix.Error
import System.Posix.Types
import System.IO.Unsafe
import Data.Bits
import System.Posix.Internals
import Foreign hiding (unsafePerformIO)
import Foreign.C

-- -----------------------------------------------------------------------------
-- POSIX file modes

-- The abstract type 'FileMode', constants and operators for
-- manipulating the file modes defined by POSIX.

-- | No permissions.
nullFileMode :: FileMode
nullFileMode = 0

-- | Owner has read permission.
ownerReadMode :: FileMode
ownerReadMode = (256)
{-# LINE 110 "libraries/unix/./System/Posix/Files.hsc" #-}

-- | Owner has write permission.
ownerWriteMode :: FileMode
ownerWriteMode = (128)
{-# LINE 114 "libraries/unix/./System/Posix/Files.hsc" #-}

-- | Owner has execute permission.
ownerExecuteMode :: FileMode
ownerExecuteMode = (64)
{-# LINE 118 "libraries/unix/./System/Posix/Files.hsc" #-}

-- | Group has read permission.
groupReadMode :: FileMode
groupReadMode = (32)
{-# LINE 122 "libraries/unix/./System/Posix/Files.hsc" #-}

-- | Group has write permission.
groupWriteMode :: FileMode
groupWriteMode = (16)
{-# LINE 126 "libraries/unix/./System/Posix/Files.hsc" #-}

-- | Group has execute permission.
groupExecuteMode :: FileMode
groupExecuteMode = (8)
{-# LINE 130 "libraries/unix/./System/Posix/Files.hsc" #-}

-- | Others have read permission.
otherReadMode :: FileMode
otherReadMode = (4)
{-# LINE 134 "libraries/unix/./System/Posix/Files.hsc" #-}

-- | Others have write permission.
otherWriteMode :: FileMode
otherWriteMode = (2)
{-# LINE 138 "libraries/unix/./System/Posix/Files.hsc" #-}

-- | Others have execute permission.
otherExecuteMode :: FileMode
otherExecuteMode = (1)
{-# LINE 142 "libraries/unix/./System/Posix/Files.hsc" #-}

-- | Set user ID on execution.
setUserIDMode :: FileMode
setUserIDMode = (2048)
{-# LINE 146 "libraries/unix/./System/Posix/Files.hsc" #-}

-- | Set group ID on execution.
setGroupIDMode :: FileMode
setGroupIDMode = (1024)
{-# LINE 150 "libraries/unix/./System/Posix/Files.hsc" #-}

-- | Owner, group and others have read and write permission.
stdFileMode :: FileMode
stdFileMode = ownerReadMode  .|. ownerWriteMode .|. 
	      groupReadMode  .|. groupWriteMode .|. 
	      otherReadMode  .|. otherWriteMode

-- | Owner has read, write and execute permission.
ownerModes :: FileMode
ownerModes = (448)
{-# LINE 160 "libraries/unix/./System/Posix/Files.hsc" #-}

-- | Group has read, write and execute permission.
groupModes :: FileMode
groupModes = (56)
{-# LINE 164 "libraries/unix/./System/Posix/Files.hsc" #-}

-- | Others have read, write and execute permission.
otherModes :: FileMode
otherModes = (7)
{-# LINE 168 "libraries/unix/./System/Posix/Files.hsc" #-}

-- | Owner, group and others have read, write and execute permission.
accessModes :: FileMode
accessModes = ownerModes .|. groupModes .|. otherModes

-- | Combines the two file modes into one that contains modes that appear in
-- either.
unionFileModes :: FileMode -> FileMode -> FileMode
unionFileModes m1 m2 = m1 .|. m2

-- | Combines two file modes into one that only contains modes that appear in
-- both.
intersectFileModes :: FileMode -> FileMode -> FileMode
intersectFileModes m1 m2 = m1 .&. m2

fileTypeModes :: FileMode
fileTypeModes = (61440)
{-# LINE 185 "libraries/unix/./System/Posix/Files.hsc" #-}

blockSpecialMode :: FileMode
blockSpecialMode = (24576)
{-# LINE 188 "libraries/unix/./System/Posix/Files.hsc" #-}

characterSpecialMode :: FileMode
characterSpecialMode = (8192)
{-# LINE 191 "libraries/unix/./System/Posix/Files.hsc" #-}

namedPipeMode :: FileMode
namedPipeMode = (4096)
{-# LINE 194 "libraries/unix/./System/Posix/Files.hsc" #-}

regularFileMode :: FileMode
regularFileMode = (32768)
{-# LINE 197 "libraries/unix/./System/Posix/Files.hsc" #-}

directoryMode :: FileMode
directoryMode = (16384)
{-# LINE 200 "libraries/unix/./System/Posix/Files.hsc" #-}

symbolicLinkMode :: FileMode
symbolicLinkMode = (40960)
{-# LINE 203 "libraries/unix/./System/Posix/Files.hsc" #-}

socketMode :: FileMode
socketMode = (49152)
{-# LINE 206 "libraries/unix/./System/Posix/Files.hsc" #-}

-- | @setFileMode path mode@ changes permission of the file given by @path@
-- to @mode@. This operation may fail with 'throwErrnoPathIfMinus1_' if @path@
-- doesn't exist or if the effective user ID of the current process is not that
-- of the file's owner.
--
-- Note: calls @chmod@.
setFileMode :: FilePath -> FileMode -> IO ()
setFileMode name m =
  withCString name $ \s -> do
    throwErrnoPathIfMinus1_ "setFileMode" name (c_chmod s m)

-- | @setFdMode fd mode@ acts like 'setFileMode' but uses a file descriptor
-- @fd@ instead of a 'FilePath'.
--
-- Note: calls @fchmod@.
setFdMode :: Fd -> FileMode -> IO ()
setFdMode (Fd fd) m =
  throwErrnoIfMinus1_ "setFdMode" (c_fchmod fd m)

foreign import ccall unsafe "fchmod" 
  c_fchmod :: CInt -> CMode -> IO CInt

-- | @setFileCreationMask mode@ sets the file mode creation mask to @mode@.
-- Modes set by this operation are subtracted from files and directories upon
-- creation. The previous file creation mask is returned.
--
-- Note: calls @umask@.
setFileCreationMask :: FileMode -> IO FileMode
setFileCreationMask mask = c_umask mask

-- -----------------------------------------------------------------------------
-- access()

-- | @fileAccess name read write exec@ checks if the file (or other file system
-- object) @name@ can be accessed for reading, writing and\/or executing. To
-- check a permission set the corresponding argument to 'True'.
--
-- Note: calls @access@.
fileAccess :: FilePath -> Bool -> Bool -> Bool -> IO Bool
fileAccess name readOK writeOK execOK = access name flags
  where
   flags   = read_f .|. write_f .|. exec_f
   read_f  = if readOK  then (4) else 0
{-# LINE 250 "libraries/unix/./System/Posix/Files.hsc" #-}
   write_f = if writeOK then (2) else 0
{-# LINE 251 "libraries/unix/./System/Posix/Files.hsc" #-}
   exec_f  = if execOK  then (1) else 0
{-# LINE 252 "libraries/unix/./System/Posix/Files.hsc" #-}

-- | Checks for the existence of the file.
--
-- Note: calls @access@.
fileExist :: FilePath -> IO Bool
fileExist name = 
  withCString name $ \s -> do
    r <- c_access s (0)
{-# LINE 260 "libraries/unix/./System/Posix/Files.hsc" #-}
    if (r == 0)
	then return True
	else do err <- getErrno
	        if (err == eNOENT)
		   then return False
		   else throwErrnoPath "fileExist" name

access :: FilePath -> CMode -> IO Bool
access name flags = 
  withCString name $ \s -> do
    r <- c_access s (fromIntegral flags)
    if (r == 0)
	then return True
	else do err <- getErrno
	        if (err == eACCES)
		   then return False
		   else throwErrnoPath "fileAccess" name

-- -----------------------------------------------------------------------------
-- stat() support

-- | POSIX defines operations to get information, such as owner, permissions,
-- size and access times, about a file. This information is represented by the
-- 'FileStatus' type.
--
-- Note: see @chmod@.
newtype FileStatus = FileStatus (ForeignPtr CStat)

-- | ID of the device on which this file resides.
deviceID         :: FileStatus -> DeviceID
-- | inode number
fileID           :: FileStatus -> FileID
-- | File mode (such as permissions).
fileMode         :: FileStatus -> FileMode
-- | Number of hard links to this file.
linkCount        :: FileStatus -> LinkCount
-- | ID of owner.
fileOwner        :: FileStatus -> UserID
-- | ID of group.
fileGroup        :: FileStatus -> GroupID
-- | Describes the device that this file represents.
specialDeviceID  :: FileStatus -> DeviceID
-- | Size of the file in bytes. If this file is a symbolic link the size is
-- the length of the pathname it contains.
fileSize         :: FileStatus -> FileOffset
-- | Time of last access.
accessTime       :: FileStatus -> EpochTime
-- | Time of last modification.
modificationTime :: FileStatus -> EpochTime
-- | Time of last status change (i.e. owner, group, link count, mode, etc.).
statusChangeTime :: FileStatus -> EpochTime

deviceID (FileStatus stat) = 
  unsafePerformIO $ withForeignPtr stat $ ((\hsc_ptr -> peekByteOff hsc_ptr 0))
{-# LINE 314 "libraries/unix/./System/Posix/Files.hsc" #-}
fileID (FileStatus stat) = 
  unsafePerformIO $ withForeignPtr stat $ ((\hsc_ptr -> peekByteOff hsc_ptr 8))
{-# LINE 316 "libraries/unix/./System/Posix/Files.hsc" #-}
fileMode (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ ((\hsc_ptr -> peekByteOff hsc_ptr 24))
{-# LINE 318 "libraries/unix/./System/Posix/Files.hsc" #-}
linkCount (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ ((\hsc_ptr -> peekByteOff hsc_ptr 16))
{-# LINE 320 "libraries/unix/./System/Posix/Files.hsc" #-}
fileOwner (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ ((\hsc_ptr -> peekByteOff hsc_ptr 28))
{-# LINE 322 "libraries/unix/./System/Posix/Files.hsc" #-}
fileGroup (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ ((\hsc_ptr -> peekByteOff hsc_ptr 32))
{-# LINE 324 "libraries/unix/./System/Posix/Files.hsc" #-}
specialDeviceID (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ ((\hsc_ptr -> peekByteOff hsc_ptr 40))
{-# LINE 326 "libraries/unix/./System/Posix/Files.hsc" #-}
fileSize (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ ((\hsc_ptr -> peekByteOff hsc_ptr 48))
{-# LINE 328 "libraries/unix/./System/Posix/Files.hsc" #-}
accessTime (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ ((\hsc_ptr -> peekByteOff hsc_ptr 72))
{-# LINE 330 "libraries/unix/./System/Posix/Files.hsc" #-}
modificationTime (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ ((\hsc_ptr -> peekByteOff hsc_ptr 88))
{-# LINE 332 "libraries/unix/./System/Posix/Files.hsc" #-}
statusChangeTime (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ ((\hsc_ptr -> peekByteOff hsc_ptr 104))
{-# LINE 334 "libraries/unix/./System/Posix/Files.hsc" #-}

-- | Checks if this file is a block device.
isBlockDevice     :: FileStatus -> Bool
-- | Checks if this file is a character device.
isCharacterDevice :: FileStatus -> Bool
-- | Checks if this file is a named pipe device.
isNamedPipe       :: FileStatus -> Bool
-- | Checks if this file is a regular file device.
isRegularFile     :: FileStatus -> Bool
-- | Checks if this file is a directory device.
isDirectory       :: FileStatus -> Bool
-- | Checks if this file is a symbolic link device.
isSymbolicLink    :: FileStatus -> Bool
-- | Checks if this file is a socket device.
isSocket          :: FileStatus -> Bool

isBlockDevice stat = 
  (fileMode stat `intersectFileModes` fileTypeModes) == blockSpecialMode
isCharacterDevice stat = 
  (fileMode stat `intersectFileModes` fileTypeModes) == characterSpecialMode
isNamedPipe stat = 
  (fileMode stat `intersectFileModes` fileTypeModes) == namedPipeMode
isRegularFile stat = 
  (fileMode stat `intersectFileModes` fileTypeModes) == regularFileMode
isDirectory stat = 
  (fileMode stat `intersectFileModes` fileTypeModes) == directoryMode
isSymbolicLink stat = 
  (fileMode stat `intersectFileModes` fileTypeModes) == symbolicLinkMode
isSocket stat = 
  (fileMode stat `intersectFileModes` fileTypeModes) == socketMode

-- | @getFileStatus path@ calls gets the @FileStatus@ information (user ID,
-- size, access times, etc.) for the file @path@.
--
-- Note: calls @stat@.
getFileStatus :: FilePath -> IO FileStatus
getFileStatus path = do
  fp <- mallocForeignPtrBytes (144) 
{-# LINE 372 "libraries/unix/./System/Posix/Files.hsc" #-}
  withForeignPtr fp $ \p ->
    withCString path $ \s -> 
      throwErrnoPathIfMinus1_ "getFileStatus" path (c_stat s p)
  return (FileStatus fp)

-- | @getFdStatus fd@ acts as 'getFileStatus' but uses a file descriptor @fd@.
--
-- Note: calls @fstat@.
getFdStatus :: Fd -> IO FileStatus
getFdStatus (Fd fd) = do
  fp <- mallocForeignPtrBytes (144) 
{-# LINE 383 "libraries/unix/./System/Posix/Files.hsc" #-}
  withForeignPtr fp $ \p ->
    throwErrnoIfMinus1_ "getFdStatus" (c_fstat fd p)
  return (FileStatus fp)

-- | Acts as 'getFileStatus' except when the 'FilePath' refers to a symbolic
-- link. In that case the @FileStatus@ information of the symbolic link itself
-- is returned instead of that of the file it points to.
--
-- Note: calls @lstat@.
getSymbolicLinkStatus :: FilePath -> IO FileStatus
getSymbolicLinkStatus path = do
  fp <- mallocForeignPtrBytes (144) 
{-# LINE 395 "libraries/unix/./System/Posix/Files.hsc" #-}
  withForeignPtr fp $ \p ->
    withCString path $ \s -> 
      throwErrnoPathIfMinus1_ "getSymbolicLinkStatus" path (c_lstat s p)
  return (FileStatus fp)

foreign import ccall unsafe "__hsunix_lstat" 
  c_lstat :: CString -> Ptr CStat -> IO CInt

-- | @createNamedPipe fifo mode@  
-- creates a new named pipe, @fifo@, with permissions based on
-- @mode@. May fail with 'throwErrnoPathIfMinus1_' if a file named @name@
-- already exists or if the effective user ID of the current process doesn't
-- have permission to create the pipe.
--
-- Note: calls @mkfifo@.
createNamedPipe :: FilePath -> FileMode -> IO ()
createNamedPipe name mode = do
  withCString name $ \s -> 
    throwErrnoPathIfMinus1_ "createNamedPipe" name (c_mkfifo s mode)

-- | @createDevice path mode dev@ creates either a regular or a special file
-- depending on the value of @mode@ (and @dev@).  @mode@ will normally be either
-- 'blockSpecialMode' or 'characterSpecialMode'.  May fail with
-- 'throwErrnoPathIfMinus1_' if a file named @name@ already exists or if the
-- effective user ID of the current process doesn't have permission to create
-- the file.
--
-- Note: calls @mknod@.
createDevice :: FilePath -> FileMode -> DeviceID -> IO ()
createDevice path mode dev =
  withCString path $ \s ->
    throwErrnoPathIfMinus1_ "createDevice" path (c_mknod s mode dev)

foreign import ccall unsafe "__hsunix_mknod" 
  c_mknod :: CString -> CMode -> CDev -> IO CInt

-- -----------------------------------------------------------------------------
-- Hard links

-- | @createLink old new@ creates a new path, @new@, linked to an existing file,
-- @old@.
--
-- Note: calls @link@.
createLink :: FilePath -> FilePath -> IO ()
createLink name1 name2 =
  withCString name1 $ \s1 ->
  withCString name2 $ \s2 ->
  throwErrnoPathIfMinus1_ "createLink" name1 (c_link s1 s2)

-- | @removeLink path@ removes the link named @path@.
--
-- Note: calls @unlink@.
removeLink :: FilePath -> IO ()
removeLink name =
  withCString name $ \s ->
  throwErrnoPathIfMinus1_ "removeLink" name (c_unlink s)

-- -----------------------------------------------------------------------------
-- Symbolic Links

-- | @createSymbolicLink file1 file2@ creates a symbolic link named @file2@
-- which points to the file @file1@.
--
-- Symbolic links are interpreted at run-time as if the contents of the link
-- had been substituted into the path being followed to find a file or directory.
--
-- Note: calls @symlink@.
createSymbolicLink :: FilePath -> FilePath -> IO ()
createSymbolicLink file1 file2 =
  withCString file1 $ \s1 ->
  withCString file2 $ \s2 ->
  throwErrnoPathIfMinus1_ "createSymbolicLink" file1 (c_symlink s1 s2)

foreign import ccall unsafe "symlink"
  c_symlink :: CString -> CString -> IO CInt

-- ToDo: should really use SYMLINK_MAX, but not everyone supports it yet,
-- and it seems that the intention is that SYMLINK_MAX is no larger than
-- PATH_MAX.

{-# LINE 479 "libraries/unix/./System/Posix/Files.hsc" #-}

-- | Reads the @FilePath@ pointed to by the symbolic link and returns it.
--
-- Note: calls @readlink@.
readSymbolicLink :: FilePath -> IO FilePath
readSymbolicLink file =
  allocaArray0 (4096) $ \buf -> do
{-# LINE 486 "libraries/unix/./System/Posix/Files.hsc" #-}
    withCString file $ \s -> do
      len <- throwErrnoPathIfMinus1 "readSymbolicLink" file $ 
	c_readlink s buf (4096)
{-# LINE 489 "libraries/unix/./System/Posix/Files.hsc" #-}
      peekCStringLen (buf,fromIntegral len)

foreign import ccall unsafe "readlink"
  c_readlink :: CString -> CString -> CSize -> IO CInt

-- -----------------------------------------------------------------------------
-- Renaming files

-- | @rename old new@ renames a file or directory from @old@ to @new@.
--
-- Note: calls @rename@.
rename :: FilePath -> FilePath -> IO ()
rename name1 name2 =
  withCString name1 $ \s1 ->
  withCString name2 $ \s2 ->
  throwErrnoPathIfMinus1_ "rename" name1 (c_rename s1 s2)

foreign import ccall unsafe "rename"
   c_rename :: CString -> CString -> IO CInt

-- -----------------------------------------------------------------------------
-- chown()

-- | @setOwnerAndGroup path uid gid@ changes the owner and group of @path@ to
-- @uid@ and @gid@, respectively.
--
-- If @uid@ or @gid@ is specified as -1, then that ID is not changed.
--
-- Note: calls @chown@.
setOwnerAndGroup :: FilePath -> UserID -> GroupID -> IO ()
setOwnerAndGroup name uid gid = do
  withCString name $ \s ->
    throwErrnoPathIfMinus1_ "setOwnerAndGroup" name (c_chown s uid gid)

foreign import ccall unsafe "chown"
  c_chown :: CString -> CUid -> CGid -> IO CInt

-- | Acts as 'setOwnerAndGroup' but uses a file descriptor instead of a
-- 'FilePath'.
--
-- Note: calls @fchown@.
setFdOwnerAndGroup :: Fd -> UserID -> GroupID -> IO ()
setFdOwnerAndGroup (Fd fd) uid gid = 
  throwErrnoIfMinus1_ "setFdOwnerAndGroup" (c_fchown fd uid gid)

foreign import ccall unsafe "fchown"
  c_fchown :: CInt -> CUid -> CGid -> IO CInt


{-# LINE 538 "libraries/unix/./System/Posix/Files.hsc" #-}
-- | Acts as 'setOwnerAndGroup' but does not follow symlinks (and thus
-- changes permissions on the link itself).
--
-- Note: calls @lchown@.
setSymbolicLinkOwnerAndGroup :: FilePath -> UserID -> GroupID -> IO ()
setSymbolicLinkOwnerAndGroup name uid gid = do
  withCString name $ \s ->
    throwErrnoPathIfMinus1_ "setSymbolicLinkOwnerAndGroup" name
	(c_lchown s uid gid)

foreign import ccall unsafe "lchown"
  c_lchown :: CString -> CUid -> CGid -> IO CInt

{-# LINE 551 "libraries/unix/./System/Posix/Files.hsc" #-}

-- -----------------------------------------------------------------------------
-- utime()

-- | @setFileTimes path atime mtime@ sets the access and modification times
-- associated with file @path@ to @atime@ and @mtime@, respectively.
--
-- Note: calls @utime@.
setFileTimes :: FilePath -> EpochTime -> EpochTime -> IO ()
setFileTimes name atime mtime = do
  withCString name $ \s ->
   allocaBytes (16) $ \p -> do
{-# LINE 563 "libraries/unix/./System/Posix/Files.hsc" #-}
     ((\hsc_ptr -> pokeByteOff hsc_ptr 0))  p atime
{-# LINE 564 "libraries/unix/./System/Posix/Files.hsc" #-}
     ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) p mtime
{-# LINE 565 "libraries/unix/./System/Posix/Files.hsc" #-}
     throwErrnoPathIfMinus1_ "setFileTimes" name (c_utime s p)

-- | @touchFile path@ sets the access and modification times associated with
-- file @path@ to the current time.
--
-- Note: calls @utime@.
touchFile :: FilePath -> IO ()
touchFile name = do
  withCString name $ \s ->
   throwErrnoPathIfMinus1_ "touchFile" name (c_utime s nullPtr)

-- -----------------------------------------------------------------------------
-- Setting file sizes

-- | Truncates the file down to the specified length. If the file was larger
-- than the given length before this operation was performed the extra is lost.
--
-- Note: calls @truncate@.
setFileSize :: FilePath -> FileOffset -> IO ()
setFileSize file off = 
  withCString file $ \s ->
    throwErrnoPathIfMinus1_ "setFileSize" file (c_truncate s off)

foreign import ccall unsafe "truncate"
  c_truncate :: CString -> COff -> IO CInt

-- | Acts as 'setFileSize' but uses a file descriptor instead of a 'FilePath'.
--
-- Note: calls @ftruncate@.
setFdSize :: Fd -> FileOffset -> IO ()
setFdSize (Fd fd) off =
  throwErrnoIfMinus1_ "setFdSize" (c_ftruncate fd off)

-- -----------------------------------------------------------------------------
-- pathconf()/fpathconf() support

data PathVar
  = FileSizeBits		  {- _PC_FILESIZEBITS     -}
  | LinkLimit                     {- _PC_LINK_MAX         -}
  | InputLineLimit                {- _PC_MAX_CANON        -}
  | InputQueueLimit               {- _PC_MAX_INPUT        -}
  | FileNameLimit                 {- _PC_NAME_MAX         -}
  | PathNameLimit                 {- _PC_PATH_MAX         -}
  | PipeBufferLimit               {- _PC_PIPE_BUF         -}
				  -- These are described as optional in POSIX:
  				  {- _PC_ALLOC_SIZE_MIN     -}
  				  {- _PC_REC_INCR_XFER_SIZE -}
  				  {- _PC_REC_MAX_XFER_SIZE  -}
  				  {- _PC_REC_MIN_XFER_SIZE  -}
 				  {- _PC_REC_XFER_ALIGN     -}
  | SymbolicLinkLimit		  {- _PC_SYMLINK_MAX      -}
  | SetOwnerAndGroupIsRestricted  {- _PC_CHOWN_RESTRICTED -}
  | FileNamesAreNotTruncated      {- _PC_NO_TRUNC         -}
  | VDisableChar		  {- _PC_VDISABLE         -}
  | AsyncIOAvailable		  {- _PC_ASYNC_IO         -}
  | PrioIOAvailable		  {- _PC_PRIO_IO          -}
  | SyncIOAvailable		  {- _PC_SYNC_IO          -}

pathVarConst :: PathVar -> CInt
pathVarConst v = case v of
	LinkLimit     			-> (0)
{-# LINE 626 "libraries/unix/./System/Posix/Files.hsc" #-}
	InputLineLimit			-> (1)
{-# LINE 627 "libraries/unix/./System/Posix/Files.hsc" #-}
	InputQueueLimit			-> (2)
{-# LINE 628 "libraries/unix/./System/Posix/Files.hsc" #-}
	FileNameLimit			-> (3)
{-# LINE 629 "libraries/unix/./System/Posix/Files.hsc" #-}
	PathNameLimit			-> (4)
{-# LINE 630 "libraries/unix/./System/Posix/Files.hsc" #-}
	PipeBufferLimit			-> (5)
{-# LINE 631 "libraries/unix/./System/Posix/Files.hsc" #-}
	SetOwnerAndGroupIsRestricted	-> (6)
{-# LINE 632 "libraries/unix/./System/Posix/Files.hsc" #-}
	FileNamesAreNotTruncated	-> (7)
{-# LINE 633 "libraries/unix/./System/Posix/Files.hsc" #-}
	VDisableChar			-> (8)
{-# LINE 634 "libraries/unix/./System/Posix/Files.hsc" #-}


{-# LINE 636 "libraries/unix/./System/Posix/Files.hsc" #-}
	SyncIOAvailable		-> (9)
{-# LINE 637 "libraries/unix/./System/Posix/Files.hsc" #-}

{-# LINE 640 "libraries/unix/./System/Posix/Files.hsc" #-}


{-# LINE 642 "libraries/unix/./System/Posix/Files.hsc" #-}
	AsyncIOAvailable	-> (10)
{-# LINE 643 "libraries/unix/./System/Posix/Files.hsc" #-}

{-# LINE 646 "libraries/unix/./System/Posix/Files.hsc" #-}


{-# LINE 648 "libraries/unix/./System/Posix/Files.hsc" #-}
	PrioIOAvailable		-> (11)
{-# LINE 649 "libraries/unix/./System/Posix/Files.hsc" #-}

{-# LINE 652 "libraries/unix/./System/Posix/Files.hsc" #-}


{-# LINE 656 "libraries/unix/./System/Posix/Files.hsc" #-}
	FileSizeBits		-> error "_PC_FILESIZEBITS not available"

{-# LINE 658 "libraries/unix/./System/Posix/Files.hsc" #-}


{-# LINE 662 "libraries/unix/./System/Posix/Files.hsc" #-}
	SymbolicLinkLimit	-> error "_PC_SYMLINK_MAX not available"

{-# LINE 664 "libraries/unix/./System/Posix/Files.hsc" #-}


-- | @getPathVar var path@ obtains the dynamic value of the requested
-- configurable file limit or option associated with file or directory @path@.
-- For defined file limits, @getPathVar@ returns the associated
-- value.  For defined file options, the result of @getPathVar@
-- is undefined, but not failure.
--
-- Note: calls @pathconf@.
getPathVar :: FilePath -> PathVar -> IO Limit
getPathVar name v = do
  withCString name $ \ nameP -> 
    throwErrnoPathIfMinus1 "getPathVar" name $ 
      c_pathconf nameP (pathVarConst v)

foreign import ccall unsafe "pathconf" 
  c_pathconf :: CString -> CInt -> IO CLong


-- | @getFdPathVar var fd@ obtains the dynamic value of the requested
-- configurable file limit or option associated with the file or directory
-- attached to the open channel @fd@. For defined file limits, @getFdPathVar@
-- returns the associated value.  For defined file options, the result of
-- @getFdPathVar@ is undefined, but not failure.
--
-- Note: calls @fpathconf@.
getFdPathVar :: Fd -> PathVar -> IO Limit
getFdPathVar (Fd fd) v =
    throwErrnoIfMinus1 "getFdPathVar" $ 
      c_fpathconf fd (pathVarConst v)

foreign import ccall unsafe "fpathconf" 
  c_fpathconf :: CInt -> CInt -> IO CLong
