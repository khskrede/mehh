module Paths_array (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,3,0,2], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/array-0.3.0.2/ghc-7.0.3"
datadir    = "/usr/local/share/array-0.3.0.2"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "array_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "array_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "array_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "array_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
