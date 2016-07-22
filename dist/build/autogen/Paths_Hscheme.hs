module Paths_Hscheme (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/i/.cabal/bin"
libdir     = "/home/i/.cabal/lib/x86_64-linux-ghc-7.8.4/Hscheme-0.1.0.0"
datadir    = "/home/i/.cabal/share/x86_64-linux-ghc-7.8.4/Hscheme-0.1.0.0"
libexecdir = "/home/i/.cabal/libexec"
sysconfdir = "/home/i/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Hscheme_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Hscheme_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Hscheme_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Hscheme_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Hscheme_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
