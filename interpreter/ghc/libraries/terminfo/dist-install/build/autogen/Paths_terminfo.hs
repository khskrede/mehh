module Paths_terminfo (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,3,1,3], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/terminfo-0.3.1.3/ghc-7.0.3"
datadir    = "/usr/local/share/terminfo-0.3.1.3"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "terminfo_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "terminfo_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "terminfo_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "terminfo_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
