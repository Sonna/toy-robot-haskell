module Robot
(
  Robot (..),
  -- Entity (..),
  Robot.new,
  Robot.report
) where

-- data Robot = Coordinate Int Int String
-- report :: Robot -> Int -> Int -> String
-- data Entity = Robot Int Int String
data Robot = Robot
  { x :: Int
  , y :: Int
  , facing :: String }

new :: () -> Robot
new () = Robot
  { x = 0
  , y = 0
  , facing = "NORTH" }

-- report :: Robot -> String
-- report robot = show (x robot) ++ "," ++ show (y robot) ++  "," ++ (facing robot)

-- main = print (report $ Robot 0 0 "NORTH" )

class Entity a where
  report :: a -> String
  -- new :: () -> a

instance Entity Robot where
  report robot = show (x robot) ++ "," ++ show (y robot) ++  "," ++ (facing robot)
  -- report = show x ++ "," ++ show y ++  "," ++ facing
  -- new () = Robot 0 0 "NORTH"
    -- { x = 0
    -- , y = 0
    -- , facing = "NORTH" }
