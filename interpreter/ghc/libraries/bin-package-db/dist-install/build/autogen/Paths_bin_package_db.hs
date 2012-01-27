module Paths_bin_package_db (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,0,0,0], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/bin-package-db-0.0.0.0/ghc-7.0.3"
datadir    = "/usr/local/share/bin-package-db-0.0.0.0"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "bin_package_db_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "bin_package_db_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "bin_package_db_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "bin_package_db_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
