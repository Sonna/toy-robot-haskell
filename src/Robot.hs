module Robot
(
  Robot (..),
  Robot.new,
  Robot.new_,
  Robot.exec,
  Robot.left,
  Robot.right,
  Robot.move,
  Robot.place,
  Robot.report
) where

import Data.List.Split

data Robot = Robot
  { x :: Int
  , y :: Int
  , facing :: String }
  deriving (Show, Eq)

new :: () -> Robot
new () = Robot
  { x = 0
  , y = 0
  , facing = "NORTH" }

new_ :: () -> IO Robot
new_ () = putStr "" >> return Robot { x = 0, y = 0, facing = "NORTH" }

class Entity a where
  report :: a -> IO a
  left :: a -> a
  right :: a -> a
  move :: a -> a
  place :: a -> String -> a
  exec :: a -> String -> String -> IO a

instance Entity Robot where
  report robot =
    putStrLn (show (x robot) ++ "," ++ show (y robot) ++  "," ++ (facing robot))
    >> return robot

  left robot
    | (facing robot) == "NORTH" = robot { facing = "WEST" }
    | (facing robot) == "WEST" = robot { facing = "SOUTH" }
    | (facing robot) == "SOUTH" = robot { facing = "EAST" }
    | (facing robot) == "EAST" = robot { facing = "NORTH" }
    | otherwise = robot

  right robot
    | (facing robot) == "NORTH" = robot { facing = "EAST" }
    | (facing robot) == "EAST" = robot { facing = "SOUTH" }
    | (facing robot) == "SOUTH" = robot { facing = "WEST" }
    | (facing robot) == "WEST" = robot { facing = "NORTH" }
    | otherwise = robot

  move robot
    | (facing robot) == "NORTH" && yNorth < 4 = robot { y = yNorth }
    | (facing robot) == "SOUTH" && ySouth > -1 = robot { y = ySouth }
    | (facing robot) == "EAST" && xEast < 4 = robot { x = xEast }
    | (facing robot) == "WEST" && xWest > -1 = robot { x = xWest }
    | otherwise = robot
    where yNorth = y robot + 1
          ySouth = y robot - 1
          xEast = x robot + 1
          xWest = x robot - 1

  place robot rawCoordinates = robot {
    x = (read newX),
    y = (read newY),
    facing = newFacing }
    where [newX, newY, newFacing] = splitOn "," rawCoordinates

  exec robot rawCommand rawArgs
    | rawCommand == "PLACE" = blank >> return (place robot rawArgs)
    | rawCommand == "MOVE" = blank >> return (move robot)
    | rawCommand == "LEFT" = blank >> return (left robot)
    | rawCommand == "RIGHT" = blank >> return (right robot)
    | rawCommand == "REPORT" = report robot
    | otherwise = blank >> return robot
    where blank = putStr ""

