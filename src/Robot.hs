module Robot
(
  -- Robot (..),
  Entity (..),
  new,
  report
) where

-- data Robot = Coordinate Int Int String
-- report :: Robot -> Int -> Int -> String
data Entity = Robot Int Int String

new :: () -> Entity
new () = Robot 0 0 "NORTH"

report :: Entity -> String
report (Robot x y facing) = show x ++ "," ++ show y ++  "," ++ facing

-- main = print (report $ Robot 0 0 "NORTH" )
