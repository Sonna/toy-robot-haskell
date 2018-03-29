module Robot
(
  Robot (..),
  Robot.new,
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

new :: () -> Robot
new () = Robot
  { x = 0
  , y = 0
  , facing = "NORTH" }


class Entity a where
  report :: a -> IO ()
  left :: a -> a
  right :: a -> a
  move :: a -> a
  place :: a -> String -> a

instance Entity Robot where
  report robot = putStrLn $
    show (x robot) ++ "," ++ show (y robot) ++  "," ++ (facing robot)

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
    | (facing robot) == "SOUTH" && ySouth > 0 = robot { y = ySouth }
    | (facing robot) == "EAST" && xEast < 4 = robot { x = xEast }
    | (facing robot) == "WEST" && xWest > 0 = robot { x = xWest }
    | otherwise = robot
    where yNorth = y robot + 1
          ySouth = y robot - 1
          xEast = x robot + 1
          xWest = x robot - 1

  place robot rawCoordinates = robot { x = (read newX), y = (read newY), facing = newFacing }
    where [newX, newY, newFacing] = splitOn "," rawCoordinates
