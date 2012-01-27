module Paths_haskeline (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,6,3], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/haskeline-0.6.3/ghc-7.0.3"
datadir    = "/usr/local/share/haskeline-0.6.3"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "haskeline_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "haskeline_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "haskeline_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "haskeline_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
