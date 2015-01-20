module Paths_Hash (
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
version = Version {versionBranch = [0,1], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/shtakor/hs2/.metadata/.plugins/net.sf.eclipsefp.haskell.buildwrapper/sandbox//bin"
libdir     = "/home/shtakor/hs2/.metadata/.plugins/net.sf.eclipsefp.haskell.buildwrapper/sandbox//lib/Hash-0.1/ghc-7.6.3"
datadir    = "/home/shtakor/hs2/.metadata/.plugins/net.sf.eclipsefp.haskell.buildwrapper/sandbox//share/x86_64-linux-ghc-7.6.3/Hash-0.1"
libexecdir = "/home/shtakor/hs2/.metadata/.plugins/net.sf.eclipsefp.haskell.buildwrapper/sandbox//libexec"
sysconfdir = "/home/shtakor/hs2/.metadata/.plugins/net.sf.eclipsefp.haskell.buildwrapper/sandbox//etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Hash_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Hash_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Hash_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Hash_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Hash_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
