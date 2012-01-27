module Paths_process (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [1,0,1,5], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/process-1.0.1.5/ghc-7.0.3"
datadir    = "/usr/local/share/process-1.0.1.5"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "process_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "process_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "process_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "process_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
