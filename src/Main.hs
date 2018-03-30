module Main where

import Robot
import System.IO
import System.Environment
import Data.List

main :: IO ()
main = do
  let robot = new()

  args <- getArgs
  case args of
      [file] -> withFile (head args) ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr contents)
      [] -> userLoop robot >> putStr ""
      _ -> putStr "Do Nothing"

userLoop :: Robot -> IO Robot
userLoop robot = do
  rawCommands <- readCommandLine
  if ((head rawCommands) == "EXIT")
    then putStr "" >> return robot -- Need to fix issue later
    else do
        let (command, commandArgs) = case rawCommands of
              [rawCommand, rawCommandArgs] -> (rawCommand, rawCommandArgs)
              [rawCommand] -> (rawCommand, "")
              _ -> ("", "")

        exec robot command commandArgs >>= \newRobot -> userLoop newRobot
        -- putStr "" -- Need to fix issue later

readCommandLine :: IO [String]
readCommandLine = fmap (words) getLine
