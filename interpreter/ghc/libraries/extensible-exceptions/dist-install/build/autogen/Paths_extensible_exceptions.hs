module Paths_extensible_exceptions (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,1,1,2], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/extensible-exceptions-0.1.1.2/ghc-7.0.3"
datadir    = "/usr/local/share/extensible-exceptions-0.1.1.2"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "extensible_exceptions_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "extensible_exceptions_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "extensible_exceptions_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "extensible_exceptions_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
