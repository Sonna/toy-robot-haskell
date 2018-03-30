module Main where

import Robot

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  let robot = new()
  subject <- report robot
  print subject
