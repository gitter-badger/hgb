{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_hgb (
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

bindir     = "/Users/lukasberglund/Documents/Code/hgb/.stack-work/install/x86_64-osx/ee76c23dc14bf08d60e45fb054dbb8f102f2da1cd9172cd386046771b7e71e30/8.6.5/bin"
libdir     = "/Users/lukasberglund/Documents/Code/hgb/.stack-work/install/x86_64-osx/ee76c23dc14bf08d60e45fb054dbb8f102f2da1cd9172cd386046771b7e71e30/8.6.5/lib/x86_64-osx-ghc-8.6.5/hgb-0.1.0.0-GPArUDO4tdHK7JXmD3wQwb-hgb-exe"
dynlibdir  = "/Users/lukasberglund/Documents/Code/hgb/.stack-work/install/x86_64-osx/ee76c23dc14bf08d60e45fb054dbb8f102f2da1cd9172cd386046771b7e71e30/8.6.5/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/lukasberglund/Documents/Code/hgb/.stack-work/install/x86_64-osx/ee76c23dc14bf08d60e45fb054dbb8f102f2da1cd9172cd386046771b7e71e30/8.6.5/share/x86_64-osx-ghc-8.6.5/hgb-0.1.0.0"
libexecdir = "/Users/lukasberglund/Documents/Code/hgb/.stack-work/install/x86_64-osx/ee76c23dc14bf08d60e45fb054dbb8f102f2da1cd9172cd386046771b7e71e30/8.6.5/libexec/x86_64-osx-ghc-8.6.5/hgb-0.1.0.0"
sysconfdir = "/Users/lukasberglund/Documents/Code/hgb/.stack-work/install/x86_64-osx/ee76c23dc14bf08d60e45fb054dbb8f102f2da1cd9172cd386046771b7e71e30/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hgb_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hgb_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hgb_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hgb_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hgb_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hgb_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
