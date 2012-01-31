module Paths_time (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [1,2,0,3], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/time-1.2.0.3/ghc-7.0.3"
datadir    = "/usr/local/share/time-1.2.0.3"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "time_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "time_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "time_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "time_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)