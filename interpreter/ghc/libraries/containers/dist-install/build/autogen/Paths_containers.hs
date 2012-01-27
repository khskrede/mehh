module Paths_containers (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,4,0,0], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/containers-0.4.0.0/ghc-7.0.3"
datadir    = "/usr/local/share/containers-0.4.0.0"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "containers_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "containers_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "containers_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "containers_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
