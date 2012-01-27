module Paths_directory (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [1,1,0,0], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/directory-1.1.0.0/ghc-7.0.3"
datadir    = "/usr/local/share/directory-1.1.0.0"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "directory_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "directory_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "directory_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "directory_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
