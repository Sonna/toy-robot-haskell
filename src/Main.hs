module Main where

import ToyRobot.CLI
import System.Environment

main :: IO ()
main = getArgs >>= run >> putStr ""
