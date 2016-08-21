import SpecHelper

main :: IO ()
main = hspec $ do
  describe "heartRate" $ do
    it "returns Nothing if wrong input" $ do
      heartRate "what?" `shouldBe` Nothing
      heartRate "180" `shouldBe` (Just 180 :: Maybe Int)

  describe "zone" $ do
    it "returns a tuple of  min and max heart rate" $ do
      zone 180 1 `shouldBe` (1,122)
      zone 180 2 `shouldBe` (123,149)
      zone 180 3 `shouldBe` (150,169)
      zone 180 4 `shouldBe` (170,189)
      zone 180 5 `shouldBe` (190,0)

    it "throws exception when zone is out of range" $ do
      evaluate (zone 180 0) `shouldThrow` anyErrorCall
      evaluate (zone 180 1000) `shouldThrow` anyErrorCall

  describe "formatZone" $ do
    it "returns a string of  min and max heart rate" $ do
      formatZone 180 1 `shouldBe` "<= 122"
      formatZone 180 2 `shouldBe` "123-149"
      formatZone 180 3 `shouldBe` "150-169"
      formatZone 180 4 `shouldBe` "170-189"
      formatZone 180 5 `shouldBe` "> 190"
