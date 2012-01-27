module Paths_random (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [1,0,0,3], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/random-1.0.0.3/ghc-7.0.3"
datadir    = "/usr/local/share/random-1.0.0.3"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "random_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "random_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "random_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "random_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
