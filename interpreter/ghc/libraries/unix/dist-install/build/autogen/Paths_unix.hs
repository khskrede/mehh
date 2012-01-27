module Paths_unix (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [2,4,2,0], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/unix-2.4.2.0/ghc-7.0.3"
datadir    = "/usr/local/share/unix-2.4.2.0"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "unix_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "unix_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "unix_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "unix_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
