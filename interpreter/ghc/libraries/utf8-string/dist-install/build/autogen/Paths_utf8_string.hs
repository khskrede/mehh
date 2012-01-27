module Paths_utf8_string (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,3,6], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/utf8-string-0.3.6/ghc-7.0.3"
datadir    = "/usr/local/share/utf8-string-0.3.6"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "utf8_string_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "utf8_string_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "utf8_string_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "utf8_string_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
