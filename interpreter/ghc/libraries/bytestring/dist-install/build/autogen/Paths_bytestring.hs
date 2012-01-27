module Paths_bytestring (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,9,1,10], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/bytestring-0.9.1.10/ghc-7.0.3"
datadir    = "/usr/local/share/bytestring-0.9.1.10"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "bytestring_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "bytestring_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "bytestring_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "bytestring_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
