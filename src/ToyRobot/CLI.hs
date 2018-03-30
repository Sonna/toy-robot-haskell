module ToyRobot.CLI
(
  run,
  fileLoop,
  userLoop,
  execCommand,
  tuplifyInput
) where

import Robot
import System.IO

run :: [String] -> IO Robot
run args = do
  let robot = new()

  case args of
      [filename] -> openFile filename ReadMode >>= \file -> fileLoop file robot
      [] -> userLoop robot
      _ -> putStr "Too many or not few enough Arguments" >> return robot

fileLoop :: Handle -> Robot -> IO Robot
fileLoop file robot = hIsEOF file >>= \ineof ->
  if ineof
    then putStr "" >> return robot
    else do
      hGetLine file >>=
        execCommand robot >>=
          fileLoop file

userLoop :: Robot -> IO Robot
userLoop robot = getLine >>= \line ->
  case (words line) of
    ["EXIT"] -> putStr "" >> return robot
    _ -> execCommand robot line >>= userLoop

execCommand :: Robot -> String -> IO Robot
execCommand robot line = exec robot command commandArgs
  where (command, commandArgs) = tuplifyInput (words line)

tuplifyInput :: [String] -> (String, String)
tuplifyInput [rawCommand, rawCommandArgs] = (rawCommand, rawCommandArgs)
tuplifyInput [rawCommand] = (rawCommand, "")
tuplifyInput _ = ("", "")
