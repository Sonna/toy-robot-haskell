module RobotSpec where

import Robot
import TestHelpers
import Test.Hspec

spec :: Spec
spec = do

  describe "Robot constructor" $ do
    it "can be constructed without values" $ do
      let subject = new()

      (x subject) `shouldBe` 0
      (y subject) `shouldBe` 0
      (facing subject) `shouldBe` "NORTH"

    it "can be constructed with values" $ do
      let subject = Robot 0 0 "NORTH"

      (x subject) `shouldBe` 0
      (y subject) `shouldBe` 0
      (facing subject) `shouldBe` "NORTH"

  describe "Validate report function" $ do
    it "report is supposed to return x, y and facing direction" $ do
      subject <- catchOutput (report $ Robot 0 0 "NORTH" )
      subject `shouldBe` "0,0,NORTH\n"

  describe "left function" $ do
    it "left rotates the facing direction anti-clockwise" $ do
      let robot = Robot 0 0 "NORTH"
      let subject = left robot

      (x subject) `shouldBe` 0
      (y subject) `shouldBe` 0
      (facing subject) `shouldBe` "WEST"

    it "left rotates facing direction to SOUTH from WEST" $ do
      let robot = Robot 0 0 "WEST"
      let subject = left robot

      (x subject) `shouldBe` 0
      (y subject) `shouldBe` 0
      (facing subject) `shouldBe` "SOUTH"

    it "left rotates facing direction to EAST from SOUTH" $ do
      let robot = Robot 0 0 "SOUTH"
      let subject = left robot

      (x subject) `shouldBe` 0
      (y subject) `shouldBe` 0
      (facing subject) `shouldBe` "EAST"

    it "left rotates facing direction to NORTH from EAST" $ do
      let robot = Robot 0 0 "EAST"
      let subject = left robot

      (x subject) `shouldBe` 0
      (y subject) `shouldBe` 0
      (facing subject) `shouldBe` "NORTH"
