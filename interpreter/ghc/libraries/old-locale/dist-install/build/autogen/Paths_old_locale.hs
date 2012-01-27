module Paths_old_locale (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [1,0,0,2], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/old-locale-1.0.0.2/ghc-7.0.3"
datadir    = "/usr/local/share/old-locale-1.0.0.2"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "old_locale_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "old_locale_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "old_locale_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "old_locale_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
