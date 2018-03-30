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
fileLoop robot file = hIsEOF file >>= \ineof ->
  if ineof
    then putStr "" >> return robot
    else do
      hGetLine file >>= \line ->
        execCommand robot (words line) >>= \newRobot ->
          fileLoop newRobot file

userLoop :: Robot -> IO Robot
userLoop robot = getLine >>= \line ->
  case (words line) of
    ["EXIT"] -> putStr "" >> return robot
    _ -> execCommand robot (words line) >>= \newRobot -> userLoop newRobot

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
