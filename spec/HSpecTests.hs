import Test.Hspec

import qualified HaqSpec
import qualified RobotSpec
import qualified ToyRobot.CLISpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Haq"     HaqSpec.spec
  describe "Robot"     RobotSpec.spec
  describe "ToyRobot.CLI" ToyRobot.CLISpec.spec
