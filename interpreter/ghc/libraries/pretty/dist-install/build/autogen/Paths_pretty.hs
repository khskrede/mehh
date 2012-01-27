module Paths_pretty (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [1,0,1,2], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/pretty-1.0.1.2/ghc-7.0.3"
datadir    = "/usr/local/share/pretty-1.0.1.2"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "pretty_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "pretty_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "pretty_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "pretty_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
