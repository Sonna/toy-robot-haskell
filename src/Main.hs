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
    else execCommand robot rawCommands >>= \newRobot -> userLoop newRobot

execCommand :: Robot -> [String] -> IO Robot
execCommand robot commandList = exec robot command commandArgs
  where (command, commandArgs) = tuplifyInput commandList

tuplifyInput :: [String] -> (String, String)
tuplifyInput wordList =
  case wordList of
      [rawCommand, rawCommandArgs] -> (rawCommand, rawCommandArgs)
      [rawCommand] -> (rawCommand, "")
      _ -> ("", "")

readCommandLine :: IO [String]
readCommandLine = fmap (words) getLine
