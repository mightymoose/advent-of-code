import Test.Hspec

import FirstDay

main :: IO ()
main = hspec $
  describe "FirstDay" firstDaySpec

firstDaySpec :: Spec
firstDaySpec = do
  describe "FirstDay.partTwoSolution" $
    it "is 5084676" $
      partTwoSolution `shouldBe` 5084676
  describe "FirstDay.partOneSolution" $
    it "is 3391707" $
      partOneSolution `shouldBe` 3391707
  describe "FirstDay.gasForPayload" $ do
    it "is 2 for a payload of 12" $
      gasForPayload 12 `shouldBe` 2
    it "is 2 for a payload of 14" $
      gasForPayload 14 `shouldBe` 2
    it "is 654 for a payload of 1969" $
      gasForPayload 1969 `shouldBe` 654
    it "is 33583 for a payload of 100756" $
      gasForPayload 100756 `shouldBe` 33583
  describe "FirstDay.gasForGas" $ do
    it "is 0 for a gas payload of 2" $
      gasForGas 2 `shouldBe` 0
    it "is 966 for a gas payload of 654" $
      gasForGas 654 `shouldBe` 216 + 70 + 21 + 5
    it "is 2 for a gas payload of 2" $
     gasForGas 33583 `shouldBe` 11192 + 3728 + 1240 + 411 + 135 + 43 + 12 + 2
  describe "FirstDay.totalGasRequired" $
    it "is 50346 for a payload of 100756" $
     totalGasRequired 100756 `shouldBe` 50346
