{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_chess_app (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/luksu/.cabal/bin"
libdir     = "/home/luksu/.cabal/lib/x86_64-linux-ghc-8.10.2/chess-app-0.1.0.0-inplace-chess-app"
dynlibdir  = "/home/luksu/.cabal/lib/x86_64-linux-ghc-8.10.2"
datadir    = "/home/luksu/.cabal/share/x86_64-linux-ghc-8.10.2/chess-app-0.1.0.0"
libexecdir = "/home/luksu/.cabal/libexec/x86_64-linux-ghc-8.10.2/chess-app-0.1.0.0"
sysconfdir = "/home/luksu/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "chess_app_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "chess_app_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "chess_app_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "chess_app_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "chess_app_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "chess_app_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
