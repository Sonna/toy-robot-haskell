module Main where

import Robot
import System.IO
import System.Environment
import Data.List

main :: IO ()
main = do
  let robot = new()

  getArgs >>= \args -> case args of
      [file] -> openFile (head args) ReadMode >>= \file -> fileLoop file robot
      [] -> userLoop robot
      _ -> putStr "Too many or not few enough Arguments" >> return robot

  putStr ""

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
tuplifyInput wordList =
  case wordList of
      [rawCommand, rawCommandArgs] -> (rawCommand, rawCommandArgs)
      [rawCommand] -> (rawCommand, "")
      _ -> ("", "")

readCommandLine :: IO [String]
readCommandLine = fmap (words) getLine
