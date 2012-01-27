module Paths_base (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [4,3,1,0], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/base-4.3.1.0/ghc-7.0.3"
datadir    = "/usr/local/share/base-4.3.1.0"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "base_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "base_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "base_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "base_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
