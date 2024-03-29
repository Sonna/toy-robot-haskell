module RobotSpec where

import Robot
import TestHelpers
-- import System.IO.Silently
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
      -- subject <- catchOutput (report $ Robot 0 0 "NORTH" )
      -- subject `shouldBe` "0,0,NORTH\n"
      -- (subject, action) <- catchOutput (report $ Robot 0 0 "NORTH" )
      (captured, result) <- capture (report $ Robot 0 0 "NORTH" )
      captured `shouldBe` "0,0,NORTH\n"
      result `shouldBe` (Robot 0 0 "NORTH")
      -- subject <- report $ Robot 0 0 "NORTH"

      -- (x subject) `shouldBe` 0
      -- (y subject) `shouldBe` 0
      -- (facing subject) `shouldBe` "NORTH"

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

  describe "right function" $ do
    it "right rotates the facing direction clockwise" $ do
      let robot = Robot 0 0 "NORTH"
      let subject = right robot

      (x subject) `shouldBe` 0
      (y subject) `shouldBe` 0
      (facing subject) `shouldBe` "EAST"

    it "right rotates facing direction to SOUTH from EAST" $ do
      let robot = Robot 0 0 "EAST"
      let subject = right robot

      (x subject) `shouldBe` 0
      (y subject) `shouldBe` 0
      (facing subject) `shouldBe` "SOUTH"

    it "right rotates facing direction to WEST from SOUTH" $ do
      let robot = Robot 0 0 "SOUTH"
      let subject = right robot

      (x subject) `shouldBe` 0
      (y subject) `shouldBe` 0
      (facing subject) `shouldBe` "WEST"

    it "right rotates facing direction to NORTH from WEST" $ do
      let robot = Robot 0 0 "WEST"
      let subject = right robot

      (x subject) `shouldBe` 0
      (y subject) `shouldBe` 0
      (facing subject) `shouldBe` "NORTH"

  describe "move function" $ do
    it "move increases `y` field by 1, when facing NORTH" $ do
      let robot = Robot 0 0 "NORTH"
      let subject = move robot

      (x subject) `shouldBe` 0
      (y subject) `shouldBe` 1
      (facing subject) `shouldBe` "NORTH"

    it "move works at 1 1 WEST" $ do
      let robot = Robot 1 1 "WEST"
      let subject = move robot

      (x subject) `shouldBe` 0
      (y subject) `shouldBe` 1
      (facing subject) `shouldBe` "WEST"

    it "move works at 1 1 SOUTH" $ do
      let robot = Robot 1 1 "SOUTH"
      let subject = move robot

      (x subject) `shouldBe` 1
      (y subject) `shouldBe` 0
      (facing subject) `shouldBe` "SOUTH"

    it "move decreases `y` field by 1, when facing SOUTH" $ do
      let robot = Robot 2 2 "SOUTH"
      let subject = move robot

      (x subject) `shouldBe` 2
      (y subject) `shouldBe` 1
      (facing subject) `shouldBe` "SOUTH"

    it "move increase `x` field by 1, when facing EAST" $ do
      let robot = Robot 2 2 "EAST"
      let subject = move robot

      (x subject) `shouldBe` 3
      (y subject) `shouldBe` 2
      (facing subject) `shouldBe` "EAST"

    it "move decreases `x` field by 1, when facing WEST" $ do
      let robot = Robot 2 2 "WEST"
      let subject = move robot

      (x subject) `shouldBe` 1
      (y subject) `shouldBe` 2
      (facing subject) `shouldBe` "WEST"

    it "move does not fall off 4x4 Table, when at 0 0 SOUTH" $ do
      let robot = Robot 0 0 "SOUTH"
      let subject = move robot

      (x subject) `shouldBe` 0
      (y subject) `shouldBe` 0
      (facing subject) `shouldBe` "SOUTH"

    it "move does not fall off 4x4 Table, when at 0 0 WEST" $ do
      let robot = Robot 0 0 "WEST"
      let subject = move robot

      (x subject) `shouldBe` 0
      (y subject) `shouldBe` 0
      (facing subject) `shouldBe` "WEST"

    it "move does not fall off 4x4 Table, when at 0 4 WEST" $ do
      let robot = Robot 0 4 "WEST"
      let subject = move robot

      (x subject) `shouldBe` 0
      (y subject) `shouldBe` 4
      (facing subject) `shouldBe` "WEST"

    it "move does not fall off 4x4 Table, when at 0 4 NORTH" $ do
      let robot = Robot 0 4 "NORTH"
      let subject = move robot

      (x subject) `shouldBe` 0
      (y subject) `shouldBe` 4
      (facing subject) `shouldBe` "NORTH"

    it "move does not fall off 4x4 Table, when at 4 4 NORTH" $ do
      let robot = Robot 4 4 "NORTH"
      let subject = move robot

      (x subject) `shouldBe` 4
      (y subject) `shouldBe` 4
      (facing subject) `shouldBe` "NORTH"

    it "move does not fall off 4x4 Table, when at 4 4 EAST" $ do
      let robot = Robot 4 4 "EAST"
      let subject = move robot

      (x subject) `shouldBe` 4
      (y subject) `shouldBe` 4
      (facing subject) `shouldBe` "EAST"

    it "move does not fall off 4x4 Table, when at 4 0 EAST" $ do
      let robot = Robot 4 0 "EAST"
      let subject = move robot

      (x subject) `shouldBe` 4
      (y subject) `shouldBe` 0
      (facing subject) `shouldBe` "EAST"

    it "move does not fall off 4x4 Table, when at 4 0 SOUTH" $ do
      let robot = Robot 4 0 "SOUTH"
      let subject = move robot

      (x subject) `shouldBe` 4
      (y subject) `shouldBe` 0
      (facing subject) `shouldBe` "SOUTH"

  describe "place function" $ do
    it "place Robot at 3,3,SOUTH" $ do
      let robot = Robot 0 0 "NORTH"
      let subject = place robot "3,3,SOUTH"

      (x subject) `shouldBe` 3
      (y subject) `shouldBe` 3
      (facing subject) `shouldBe` "SOUTH"

    it "place Robot at 4,2,WEST" $ do
      let robot = new()
      let subject = place robot "4,2,WEST"

      (x subject) `shouldBe` 4
      (y subject) `shouldBe` 2
      (facing subject) `shouldBe` "WEST"

  describe "exec function" $ do
    it "exec Robot place at 3,3,SOUTH" $ do
      let robot = Robot 0 0 "NORTH"
      (output, subject) <- capture(exec robot "PLACE" "3,3,SOUTH")

      output `shouldBe` ""
      (x subject) `shouldBe` 3
      (y subject) `shouldBe` 3
      (facing subject) `shouldBe` "SOUTH"

    it "exec Robot move" $ do
      let robot = new()
      (output, subject) <- capture(exec robot "MOVE" "")

      output `shouldBe` ""
      (x subject) `shouldBe` 0
      (y subject) `shouldBe` 1
      (facing subject) `shouldBe` "NORTH"

    it "exec Robot LEFT" $ do
      let robot = new()
      (output, subject) <- capture(exec robot "LEFT" "")

      output `shouldBe` ""
      (x subject) `shouldBe` 0
      (y subject) `shouldBe` 0
      (facing subject) `shouldBe` "WEST"

    it "exec Robot RIGHT" $ do
      let robot = new()
      (output, subject) <- capture(exec robot "RIGHT" "")

      output `shouldBe` ""
      (x subject) `shouldBe` 0
      (y subject) `shouldBe` 0
      (facing subject) `shouldBe` "EAST"

    it "exec Robot REPORT" $ do
      let robot = new()
      (output, subject) <- capture(exec robot "REPORT" "")

      output `shouldBe` "0,0,NORTH\n"
      (x subject) `shouldBe` 0
      (y subject) `shouldBe` 0
      (facing subject) `shouldBe` "NORTH"
