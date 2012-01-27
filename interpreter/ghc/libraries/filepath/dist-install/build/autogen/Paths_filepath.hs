module Paths_filepath (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [1,2,0,0], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/filepath-1.2.0.0/ghc-7.0.3"
datadir    = "/usr/local/share/filepath-1.2.0.0"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "filepath_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "filepath_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "filepath_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "filepath_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
