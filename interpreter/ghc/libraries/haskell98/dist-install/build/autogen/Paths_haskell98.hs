module Paths_haskell98 (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [1,1,0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/haskell98-1.1.0.1/ghc-7.0.3"
datadir    = "/usr/local/share/haskell98-1.1.0.1"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "haskell98_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "haskell98_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "haskell98_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "haskell98_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
