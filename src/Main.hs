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
      [file] -> openFile (head args) ReadMode >>= \file -> fileLoop robot file
      [] -> userLoop robot
      _ -> putStr "Too many or not few enough Arguments" >> return robot
  putStr ""

fileLoop :: Robot -> Handle -> IO Robot
fileLoop robot file = do
  ineof <- hIsEOF file
  if ineof
    then putStr "" >> return robot
    else do
      line <- hGetLine file
      let rawCommands = (words line)
      if ((head rawCommands) == "EXIT")
        then putStr "" >> return robot
        else execCommand robot rawCommands >>= \newRobot -> fileLoop newRobot file

userLoop :: Robot -> IO Robot
userLoop robot = do
  rawCommands <- readCommandLine
  if ((head rawCommands) == "EXIT")
    then putStr "" >> return robot
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
