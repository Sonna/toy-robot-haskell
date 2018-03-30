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
run [filename] = openFile filename ReadMode >>= \file -> new_() >>= fileLoop file
run [] = new_() >>= userLoop
run _ = putStr "Too many or not few enough Arguments" >> new_() >>= return

fileLoop :: Handle -> Robot -> IO Robot
fileLoop file robot = hIsEOF file >>= fileProcessLine file robot

fileProcessLine :: Handle -> Robot -> Bool -> IO Robot
fileProcessLine _ robot True = putStr "" >> return robot
fileProcessLine file robot False = hGetLine file >>= execCommand robot >>= fileLoop file

userLoop :: Robot -> IO Robot
userLoop robot = getLine >>= userProcessLine robot

userProcessLine :: Robot -> String -> IO Robot
userProcessLine robot "EXIT" = putStr "" >> return robot
userProcessLine robot line = execCommand robot line >>= userLoop
-- userProcessLine robot line = exec robot command commandArgs >>= userLoop
--   where (command, commandArgs) = tuplifyInput (words line)

execCommand :: Robot -> String -> IO Robot
execCommand robot line = exec robot command commandArgs
  where (command, commandArgs) = tuplifyInput (words line)

tuplifyInput :: [String] -> (String, String)
tuplifyInput [rawCommand, rawCommandArgs] = (rawCommand, rawCommandArgs)
tuplifyInput [rawCommand] = (rawCommand, "")
tuplifyInput _ = ("", "")
