module Robot
(
  Robot (..),
  Robot.new,
  Robot.left,
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

instance Entity Robot where
  report robot = putStrLn $
    show (x robot) ++ "," ++ show (y robot) ++  "," ++ (facing robot)

  left robot | (facing robot) == "NORTH" = robot { facing = "WEST" }
             | otherwise = robot
