module RobotSpec where

import Robot
import Test.Hspec

spec :: Spec
spec = do

  describe "Robot constructor" $ do
    it "can be constructed without values" $ do
      (report $ new() ) `shouldBe` "0,0,NORTH"

  describe "Validate report function" $ do
    it "report is supposed to return x, y and facing direction" $ do
      (report $ Robot 0 0 "NORTH" ) `shouldBe` "0,0,NORTH"
