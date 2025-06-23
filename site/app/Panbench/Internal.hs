{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CApiFFI #-}
module Panbench.Internal
  ( BenchmarkStats(..)
  , benchmark
  ) where

import Data.Int

import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

-- | Benchmarking statistics gathered by @benchmark@.
data BenchmarkStats = BenchmarkStats
  { benchUserTime :: !Int64
  -- ^ The time spent in user code, measured in nanoseconds.
  , benchSystemTime :: !Int64
  -- ^ The time spent in kernel code, measured in nanoseconds.
  , benchMaxRss :: !Int64
  -- ^ Max resident set size, measured in bytes.
  , benchExitCode :: !Int64
  -- ^ The exit code of the benchmarked executable.
  }
  deriving (Show)

instance Storable BenchmarkStats where
  sizeOf _ = 4 * sizeOf (undefined :: Int64)
  alignment _ = alignment (undefined :: Int64)
  peek sp = do
    p <- pure $ castPtr sp
    benchUserTime <- peek p
    benchSystemTime <- peekElemOff p 1
    benchMaxRss <- peekElemOff p 2
    benchExitCode <- peekElemOff p 3
    pure (BenchmarkStats {..})
  poke sp (BenchmarkStats{..}) = do
    p <- pure $ castPtr sp
    poke p benchUserTime
    pokeElemOff p 1 benchSystemTime
    pokeElemOff p 2 benchMaxRss
    pokeElemOff p 3 benchExitCode

foreign import capi "benchmark.h c_benchmark" c_benchmark :: CString -> Ptr CString -> Ptr CString -> Ptr BenchmarkStats -> IO CInt

-- | Collect benchmarking stats for a single run of an executable.
--
-- @benchmark execPath args env@ will pause the GHC RTS system,
-- run the executable at @execPath@ with arguments @args@ with environment
-- variables @env@ set, and unpause the RTS.
--
-- If the executable exits with a non-zero exit code, then this
-- is reported in the returned @BenchmarkStats@. If there was some
-- other fatal error (executable not found, out of file descriptors, etc),
-- an @IOError@ is thrown.
--
-- For documentation on benchmarking statistics gathered, see @BenchmarkStats@.
benchmark :: FilePath -> [String] -> [String] -> IO BenchmarkStats
benchmark path args env = do
  p <- malloc
  r <-
    withCString path \cpath ->
    withMany withCString args \cargs ->
    withMany withCString env \cenv ->
    withArray0 nullPtr (cpath:cargs) \cargv ->
    withArray0 nullPtr cenv \cenvp ->
      c_benchmark cpath cargv cenvp p
  if r == -1 then do
    throwErrno "Panbench.Internal.benchmark"
  else
    peek p
{-# NOINLINE benchmark #-}
