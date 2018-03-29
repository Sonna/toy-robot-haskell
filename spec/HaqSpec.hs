module HaqSpec where

-- import Robot
import Haq
import Test.Hspec

spec :: Spec
spec = do

  describe "Validate haqify function" $ do
    it "haqify is supposed to prefix Haq! to things" $ do
      haqify "me" `shouldBe` "Haq! me"
