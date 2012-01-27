module Paths_template_haskell (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [2,5,0,0], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/template-haskell-2.5.0.0/ghc-7.0.3"
datadir    = "/usr/local/share/template-haskell-2.5.0.0"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "template_haskell_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "template_haskell_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "template_haskell_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "template_haskell_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
