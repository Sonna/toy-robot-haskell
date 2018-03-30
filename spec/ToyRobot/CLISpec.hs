module ToyRobot.CLISpec where

import System.IO
import System.Process
import System.Environment
import System.FilePath.Posix

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

  describe "Validate UserLoop function" $ do
    it "run process on the command line" $ do
      (Just hin, Just hout, _, _) <-
        createProcess (proc "echo" ["hello"])
        { std_in = CreatePipe, std_out = CreatePipe }

      hPutStr hin "hello grep\ngoodbye grep"
      echoBytes <- hGetContents hout

      echoBytes `shouldBe` "hello\n"

    -- it "can get build directory from current script for following specs" $ do
    --   testsExe <- getExecutablePath
    --   let testsDir = takeDirectory testsExe
    --   let buildDir = takeDirectory testsDir
    --   let exe = buildDir ++ "/toy-robot-haskell/toy-robot-haskell"

    --   exe `shouldBe` "/Users/Sonna/Projects/haskell/toy-robot-haskell/dist/build/toy-robot-haskell/toy-robot-haskell"
    --   buildDir `shouldBe` "/Users/Sonna/Projects/haskell/toy-robot-haskell/dist/build"
    --   testsDir `shouldBe` "/Users/Sonna/Projects/haskell/toy-robot-haskell/dist/build/tests"
    --   testsExe `shouldBe` "/Users/Sonna/Projects/haskell/toy-robot-haskell/dist/build/tests/tests"

    it "run process and EXIT" $ do
      -- (Just hin, Just hout, _, _) <-
      --   createProcess (proc " dist/build/toy-robot-haskell/toy-robot-haskell" [""])
      --   { std_in = CreatePipe, std_out = CreatePipe }
      testsExe <- getExecutablePath
      let testsDir = takeDirectory testsExe
      let buildDir = takeDirectory testsDir
      let exe = buildDir ++ "/toy-robot-haskell/toy-robot-haskell"
      let filename = "spec/fixtures/process_a.txt"
      handle <- openFile filename ReadMode

      (_, Just hout, _, _) <-
        createProcess (proc exe [])
        { std_in = (UseHandle handle), std_out = CreatePipe }
      -- (Just hin, Just hout, _, _) <-
        -- createProcess (proc exe ["examples/example_a.txt"])
        -- { std_in = CreatePipe, std_out = CreatePipe }

      stdoutBytes <- hGetContents hout
      stdoutBytes `shouldBe` "0,0,NORTH\n"

    it "run process MOVE REPORT EXIT" $ do
      testsExe <- getExecutablePath
      let testsDir = takeDirectory testsExe
      let buildDir = takeDirectory testsDir
      let exe = buildDir ++ "/toy-robot-haskell/toy-robot-haskell"
      let filename = "spec/fixtures/process_b.txt"
      handle <- openFile filename ReadMode

      (_, Just hout, _, _) <-
        createProcess (proc exe [])
        { std_in = (UseHandle handle), std_out = CreatePipe }

      stdoutBytes <- hGetContents hout
      stdoutBytes `shouldBe` "0,0,NORTH\n0,1,NORTH\n"

    it "run process MOVE REPORT LEFT REPORT RIGHT RIGHT MOVE REPORT PLACE 2,4,EAST REPORT EXIT" $ do
      testsExe <- getExecutablePath
      let testsDir = takeDirectory testsExe
      let buildDir = takeDirectory testsDir
      let exe = buildDir ++ "/toy-robot-haskell/toy-robot-haskell"
      let filename = "spec/fixtures/process_c.txt"
      handle <- openFile filename ReadMode

      (_, Just hout, _, _) <-
        createProcess (proc exe [])
        { std_in = (UseHandle handle), std_out = CreatePipe }

      stdoutBytes <- hGetContents hout
      stdoutBytes `shouldBe` "0,1,NORTH\n0,1,WEST\n1,1,EAST\n2,4,EAST\n"
