module Paths_hpc (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,5,0,6], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/hpc-0.5.0.6/ghc-7.0.3"
datadir    = "/usr/local/share/hpc-0.5.0.6"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "hpc_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "hpc_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "hpc_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "hpc_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
