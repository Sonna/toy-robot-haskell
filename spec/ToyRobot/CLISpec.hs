module ToyRobot.CLISpec where

import ToyRobot.CLI
import TestHelpers
-- import System.IO.Silently
import Test.Hspec

spec :: Spec
spec = do

  describe "Validate tuplifyInput function" $ do
    it "is tuplifys a list of 2 Strings or words" $ do
      let subject = tuplifyInput ["Hello", "world"]
      subject `shouldBe` ("Hello", "world")

