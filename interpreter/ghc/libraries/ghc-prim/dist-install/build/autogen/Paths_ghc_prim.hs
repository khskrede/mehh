module Paths_ghc_prim (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,2,0,0], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/ghc-prim-0.2.0.0/ghc-7.0.3"
datadir    = "/usr/local/share/ghc-prim-0.2.0.0"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "ghc_prim_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "ghc_prim_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "ghc_prim_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "ghc_prim_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
