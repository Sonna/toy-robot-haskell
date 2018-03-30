module ToyRobot.CLISpec where

import Robot
import ToyRobot.CLI
import TestHelpers

import Test.Hspec

spec :: Spec
spec = do

  describe "Validate tuplifyInput function" $ do
    it "is tuplifys a list of 2 Strings or words" $ do
      let subject = tuplifyInput ["Hello", "world"]
      subject `shouldBe` ("Hello", "world")

    it "is tuplifys a list of 1 Strings or words" $ do
      let subject = tuplifyInput ["Hello"]
      subject `shouldBe` ("Hello", "")

    it "is tuplifys a list of 3 Strings or words" $ do
      let subject = tuplifyInput ["Hello", "world", "foobar"]
      subject `shouldBe` ("", "")

    it "is tuplifys a list of no Strings or words" $ do
      let subject = tuplifyInput []
      subject `shouldBe` ("", "")

  describe "Validate execCommand function" $ do
    it "updates a given robot when feed a line of text" $ do
      let line = "REPORT"
      let robot = Robot 0 0 "NORTH"

      (output, subject) <- capture(execCommand robot line)

      output `shouldBe` "0,0,NORTH\n"
      (x subject) `shouldBe` 0
      (y subject) `shouldBe` 0
      (facing subject) `shouldBe` "NORTH"

    it "updates a given robot when feed an unknown command in line of text" $ do
      let line = "The quick brown fox jumps over the lazy dog"
      let robot = Robot 0 0 "NORTH"

      (output, subject) <- capture(execCommand robot line)

      output `shouldBe` ""
      (x subject) `shouldBe` 0
      (y subject) `shouldBe` 0
      (facing subject) `shouldBe` "NORTH"

    it "updates a given robot when feed the PLACE command in line of text" $ do
      let line = "PLACE 4,2,SOUTH"
      let robot = Robot 0 0 "NORTH"

      (output, subject) <- capture(execCommand robot line)

      output `shouldBe` ""
      (x subject) `shouldBe` 4
      (y subject) `shouldBe` 2
      (facing subject) `shouldBe` "SOUTH"

  describe "Validate FileLoop function" $ do
    it "processes example A file" $ do
      let filename = "examples/example_a.txt"

      (output, subject) <- capture(run [filename])

      output `shouldBe` "0,0,NORTH\n"
      (x subject) `shouldBe` 0
      (y subject) `shouldBe` 0
      (facing subject) `shouldBe` "NORTH"

    it "processes example B file" $ do
      let filename = "examples/example_b.txt"

      (output, subject) <- capture(run [filename])

      output `shouldBe` "0,0,WEST\n"
      (x subject) `shouldBe` 0
      (y subject) `shouldBe` 0
      (facing subject) `shouldBe` "WEST"

    it "processes example C file" $ do
      let filename = "examples/example_c.txt"

      (output, subject) <- capture(run [filename])

      output `shouldBe` "3,3,NORTH\n"
      (x subject) `shouldBe` 3
      (y subject) `shouldBe` 3
      (facing subject) `shouldBe` "NORTH"
