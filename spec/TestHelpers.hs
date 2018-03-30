{-# OPTIONS_GHC #-}
module TestHelpers(
  catchOutput,
  capture,
  hCapture
) where

import Prelude

-- #if __GLASGOW_HASKELL__ >= 612
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
-- #else
-- import GHC.Handle (hDuplicate, hDuplicateTo)
-- #endif

import System.IO
import qualified Control.Exception as E
import Control.DeepSeq
import System.Directory (removeFile,getTemporaryDirectory)

-- == Usage:
--
--    main = catchOutput (putStr "foo")
--    -- or
--    capturedOutput <- catchOutput $ putStr "foo"
--
catchOutput :: IO () -> IO String
catchOutput f = do
  tmpd <- getTemporaryDirectory
  (tmpf, tmph) <- openTempFile tmpd "haskell_stdout"
  stdout_dup <- hDuplicate stdout
  hDuplicateTo tmph stdout
  hClose tmph
  f
  hDuplicateTo stdout_dup stdout
  str <- readFile tmpf
  removeFile tmpf
  return str

-- catchOutput :: IO a -> IO String
-- captureOutput :: IO a -> IO (String, a)
-- captureOutput f action = do
--   a <- action
--   tmpd <- getTemporaryDirectory
--   (tmpf, tmph) <- openTempFile tmpd "haskell_stdout"
--   stdout_dup <- hDuplicate stdout
--   hDuplicateTo tmph stdout
--   hClose tmph
--   f
--   hDuplicateTo stdout_dup stdout
--   str <- readFile tmpf
--   removeFile tmpf
--   return (str, a)

capture :: IO a -> IO (String, a)
capture = hCapture [stdout]

-- hCapture :: [Handle] -> IO a -> IO (String, a)
-- hCapture handles action = do
--   tmpDir <- getTemporaryDirectory
--   (tmpFile, tmpHandle) <- openTempFile tmpDir "haskell_stdout"
--   -- hSeek tmpHandle AbsoluteSeek 0
--   stdout_dup <- hDuplicate stdout
--   hDuplicateTo tmpHandle stdout
--   hClose tmpHandle
--   -- f
--   go handles
--   hDuplicateTo stdout_dup stdout
--   a <- action
--   str <- readFile tmpFile
--   removeFile tmpFile
--   str <- hGetContents tmpHandle
--   -- str `deepseq`
--   return (str,a)

hCapture :: [Handle] -> IO a -> IO (String, a)
hCapture handles action = do
  tmpDir <- getTemporaryDirectory
  E.bracket (openTempFile tmpDir "capture")
                             cleanup
                             (prepareAndRun . snd)
 where
  cleanup (tmpFile, tmpHandle) = do
    hClose tmpHandle
    removeFile tmpFile
  prepareAndRun tmpHandle = go handles
    where
      go [] = do
              a <- action
              mapM_ hFlush handles
              hSeek tmpHandle AbsoluteSeek 0
              str <- hGetContents tmpHandle
              str `deepseq` return (str,a)
      go hs = goBracket go tmpHandle hs

goBracket :: ([Handle] -> IO a) -> Handle -> [Handle] -> IO a
goBracket go tmpHandle (h:hs) = do
  buffering <- hGetBuffering h
  let redirect = do
        old <- hDuplicate h
        hDuplicateTo tmpHandle h
        return old
      restore old = do
        hDuplicateTo old h
        hSetBuffering h buffering
        hClose old
  E.bracket redirect restore (\_ -> go hs)
