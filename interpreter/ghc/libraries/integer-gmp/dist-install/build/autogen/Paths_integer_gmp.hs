module Paths_integer_gmp (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,2,0,3], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/integer-gmp-0.2.0.3/ghc-7.0.3"
datadir    = "/usr/local/share/integer-gmp-0.2.0.3"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "integer_gmp_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "integer_gmp_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "integer_gmp_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "integer_gmp_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
