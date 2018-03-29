module Robot
(
  Robot (..),
  Robot.new,
  Robot.left,
  Robot.right,
  Robot.move,
  Robot.report
) where


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
    | (facing robot) == "NORTH" && y robot + 1 < 4 = robot { y = y robot + 1 }
    | (facing robot) == "SOUTH" && y robot - 1 > 0 = robot { y = y robot - 1 }
    | (facing robot) == "EAST" && x robot + 1 < 4 = robot { x = x robot + 1 }
    | (facing robot) == "WEST" && x robot - 1 > 0 = robot { x = x robot - 1 }
    | otherwise = robot
