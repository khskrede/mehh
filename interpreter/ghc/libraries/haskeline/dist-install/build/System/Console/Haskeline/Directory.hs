{-# LINE 1 "libraries/haskeline/./System/Console/Haskeline/Directory.hsc" #-}
{- |
{-# LINE 2 "libraries/haskeline/./System/Console/Haskeline/Directory.hsc" #-}
A Unicode-aware module for interacting with files.  We just need enough to support
filename completion.  In particular, these functions will silently handle all errors
(for example, file does not exist)
-}
module System.Console.Haskeline.Directory(
                    getDirectoryContents,
                    doesDirectoryExist,
                    getHomeDirectory
                    ) where


{-# LINE 76 "libraries/haskeline/./System/Console/Haskeline/Directory.hsc" #-}

import Data.ByteString.Char8 (pack, unpack)
import qualified System.Directory as D
import Control.Exception.Extensible
import System.Console.Haskeline.Backend.IConv

getDirectoryContents :: FilePath -> IO [FilePath]
getDirectoryContents path = do
    codeset <- getCodeset
    encoder <- openEncoder codeset
    decoder <- openDecoder codeset
    dirEnc <- fmap unpack (encoder path)
    filesEnc <- handle (\(_::IOException) -> return [])
                    $ D.getDirectoryContents dirEnc
    mapM (decoder . pack) filesEnc

doesDirectoryExist :: FilePath -> IO Bool
doesDirectoryExist file = do
    codeset <- getCodeset
    encoder <- openEncoder codeset
    encoder file >>= D.doesDirectoryExist . unpack

getHomeDirectory :: IO FilePath
getHomeDirectory = do
    codeset <- getCodeset
    decoder <- openDecoder codeset
    handle (\(_::IOException) -> return "")
        $ D.getHomeDirectory >>= decoder . pack

{-# LINE 105 "libraries/haskeline/./System/Console/Haskeline/Directory.hsc" #-}
