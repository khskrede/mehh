module Paths_haskell2010 (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [1,0,0,0], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/haskell2010-1.0.0.0/ghc-7.0.3"
datadir    = "/usr/local/share/haskell2010-1.0.0.0"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "haskell2010_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "haskell2010_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "haskell2010_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "haskell2010_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
