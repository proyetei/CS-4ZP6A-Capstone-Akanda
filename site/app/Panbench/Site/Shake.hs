-- | Shake utilities for @panbench-site@.
module Panbench.Site.Shake
  ( -- $shakefileutil
    createDirectoryRecursive
  , removeFile_
  , writeBinaryFileChanged
  , writeTextFileChanged
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.IO qualified as T

import System.Directory qualified as Dir
import System.FilePath
import System.IO
import System.IO.Error

-- * Shake File Utilities
--
-- $shakefileutil
--
-- The following code was adapted from @writeFileChanged@ in @General.Extras@ in @shake-0.19.8@.
-- We need to be able to write binary files, which shake does not support OOTB.

-- | Recursively create a directory if it does not exist.
createDirectoryRecursive :: FilePath -> IO ()
createDirectoryRecursive dir = do
    x <- try @IOException $ Dir.doesDirectoryExist dir
    when (x /= Right True) $ Dir.createDirectoryIfMissing True dir

-- | Remove a file.
removeFile_ :: FilePath -> IO ()
removeFile_ x =
    Dir.removeFile x `catch` \e ->
        when (isPermissionError e) $ handle @IOException (\_ -> pure ()) $ do
            perms <- Dir.getPermissions x
            Dir.setPermissions x perms{Dir.readable = True, Dir.searchable = True, Dir.writable = True}
            Dir.removeFile x

-- | Write to a file if its contents would change, using
-- the provided reading/writing functions.
--
-- This function is not intended to be called directly: see
-- @'writeBinaryFileChanged'@ and related functions.
writeFileChangedWith
  :: (MonadIO m, Eq a)
  => (Handle -> IO a)
  -> (FilePath -> a -> IO ())
  -> FilePath
  -> a
  -> m ()
writeFileChangedWith readH writeF name x = liftIO $ do
    createDirectoryRecursive $ takeDirectory name
    exists <- Dir.doesFileExist name
    if not exists then writeF name x else do
        changed <- withFile name ReadMode $ \h -> do
            src <- readH h
            pure $! src /= x
        when changed $ do
            removeFile_ name -- symlink safety
            writeF name x
{-# INLINE writeFileChangedWith #-}

-- | Write the contents of a lazy @ByteString@ to a file if the contents of
-- the file would change.
writeBinaryFileChanged :: (MonadIO m) => FilePath -> LBS.ByteString -> m ()
writeBinaryFileChanged = writeFileChangedWith LBS.hGetContents LBS.writeFile

-- | Write the contents of a strict @Text@ to a file if the contents of
-- the file would change.
writeTextFileChanged :: (MonadIO m) => FilePath -> T.Text -> m ()
writeTextFileChanged = writeFileChangedWith T.hGetContents T.writeFile
