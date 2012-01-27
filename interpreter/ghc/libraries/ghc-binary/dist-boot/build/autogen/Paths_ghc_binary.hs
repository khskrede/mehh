module Paths_ghc_binary (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,5,0,2], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/ghc-binary-0.5.0.2/ghc-7.0.3"
datadir    = "/usr/local/share/ghc-binary-0.5.0.2"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "ghc_binary_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "ghc_binary_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "ghc_binary_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "ghc_binary_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
