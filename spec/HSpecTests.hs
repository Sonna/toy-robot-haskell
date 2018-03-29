import Test.Hspec

import qualified HaqSpec
import qualified RobotSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Haq"     HaqSpec.spec
  describe "Robot"     RobotSpec.spec
