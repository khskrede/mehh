module Paths_old_time (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [1,0,0,6], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/old-time-1.0.0.6/ghc-7.0.3"
datadir    = "/usr/local/share/old-time-1.0.0.6"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "old_time_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "old_time_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "old_time_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "old_time_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
