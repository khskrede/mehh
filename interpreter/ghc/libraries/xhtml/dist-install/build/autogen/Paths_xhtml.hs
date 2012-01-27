module Paths_xhtml (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [3000,2,0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/xhtml-3000.2.0.1/ghc-7.0.3"
datadir    = "/usr/local/share/xhtml-3000.2.0.1"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "xhtml_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "xhtml_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "xhtml_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "xhtml_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
