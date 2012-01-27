module Paths_Cabal (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [1,10,1,0], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/Cabal-1.10.1.0/ghc-7.0.3"
datadir    = "/usr/local/share/Cabal-1.10.1.0"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "Cabal_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "Cabal_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "Cabal_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "Cabal_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
