import           Lib
import           Test.Hspec

main :: IO ()
main = hspec $ do
    describe "Cube creation" $ do
        describe "Zero" $ do
            it "should convert 0 properly" $ do
                unitCube 0 4 `shouldBe` Just Cube {terms = [Zero, Zero, Zero, Zero], noOfTerms = 4, minTerms = [0]}
        describe "Four" $ do
            it "should convert 4 properly in 3 terms" $ do
                unitCube 4 3 `shouldBe` Just Cube {terms = [One, Zero, Zero], noOfTerms = 3, minTerms = [4]}
            it "should convert 4 properly in 4 terms" $ do
                unitCube 4 4 `shouldBe` Just Cube {terms = [Zero, One, Zero, Zero], noOfTerms = 4, minTerms = [4]}
        describe "Five" $ do
            it "should convert 5 properly" $ do
                unitCube 5 4 `shouldBe` Just Cube {terms = [Zero, One, Zero, One], noOfTerms = 4, minTerms = [5]}
        describe "Eight" $ do
            it "should not convert 8 with 3 terms" $ do
                unitCube 8 3 `shouldBe` Nothing
            it "should convert 8 with 4 terms" $ do
                unitCube 8 4 `shouldBe` Just Cube {terms = [One, Zero, Zero, Zero], noOfTerms = 4, minTerms = [8]}
