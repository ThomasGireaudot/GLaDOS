{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_glados (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/thomas/Documents/personal/GLaDOS/.stack-work/install/x86_64-linux/ce3df872c2a3dc21d41ed1eb2460d790d18ce184c5ebf6d4bb54a47cf9c168d5/9.2.5/bin"
libdir     = "/home/thomas/Documents/personal/GLaDOS/.stack-work/install/x86_64-linux/ce3df872c2a3dc21d41ed1eb2460d790d18ce184c5ebf6d4bb54a47cf9c168d5/9.2.5/lib/x86_64-linux-ghc-9.2.5/glados-0.1.0.0-9NJ8UfRnTEULXYEYbc31ne-glados-test"
dynlibdir  = "/home/thomas/Documents/personal/GLaDOS/.stack-work/install/x86_64-linux/ce3df872c2a3dc21d41ed1eb2460d790d18ce184c5ebf6d4bb54a47cf9c168d5/9.2.5/lib/x86_64-linux-ghc-9.2.5"
datadir    = "/home/thomas/Documents/personal/GLaDOS/.stack-work/install/x86_64-linux/ce3df872c2a3dc21d41ed1eb2460d790d18ce184c5ebf6d4bb54a47cf9c168d5/9.2.5/share/x86_64-linux-ghc-9.2.5/glados-0.1.0.0"
libexecdir = "/home/thomas/Documents/personal/GLaDOS/.stack-work/install/x86_64-linux/ce3df872c2a3dc21d41ed1eb2460d790d18ce184c5ebf6d4bb54a47cf9c168d5/9.2.5/libexec/x86_64-linux-ghc-9.2.5/glados-0.1.0.0"
sysconfdir = "/home/thomas/Documents/personal/GLaDOS/.stack-work/install/x86_64-linux/ce3df872c2a3dc21d41ed1eb2460d790d18ce184c5ebf6d4bb54a47cf9c168d5/9.2.5/etc"

getBinDir     = catchIO (getEnv "glados_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "glados_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "glados_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "glados_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "glados_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "glados_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
