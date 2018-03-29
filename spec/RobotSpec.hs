module RobotSpec where

import Robot
import TestHelpers
import Test.Hspec

spec :: Spec
spec = do

  describe "Robot constructor" $ do
    it "can be constructed without values" $ do
      subject <- catchOutput (report $ new() )
      subject `shouldBe` "0,0,NORTH\n"

  describe "Validate report function" $ do
    it "report is supposed to return x, y and facing direction" $ do
      subject <- catchOutput (report $ Robot 0 0 "NORTH" )
      subject `shouldBe` "0,0,NORTH\n"
