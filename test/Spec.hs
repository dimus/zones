import SpecHelper

main :: IO ()
main = hspec $ do
  describe "zone" $ do
    it "returns a string of min and max heart rate" $ do
      zone 180 1 `shouldBe` "90-122"
