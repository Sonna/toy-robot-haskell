module TestHelpers where

import GHC.IO.Handle
import System.IO
import System.Directory

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
