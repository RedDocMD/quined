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

    describe "Can or Cannot join cubes" $ do
        describe "can join with no DC's" $ do
            it "should work when middle position differs" $ do
                let cube1 = Cube {terms = [Zero, Zero, Zero, One], noOfTerms = 4, minTerms = [1]}
                let cube2 = Cube {terms = [Zero, Zero, One, One], noOfTerms = 4, minTerms = [3]}
                canJoinCube cube1 cube2 `shouldBe` True
            it "should work when first position differs" $ do
                let cube1 = Cube {terms = [Zero, Zero, Zero, One], noOfTerms = 4, minTerms = [1]}
                let cube2 = Cube {terms = [One, Zero, Zero, One], noOfTerms = 4, minTerms = [9]}
                canJoinCube cube1 cube2 `shouldBe` True
            it "should work when last position differs" $ do
                let cube1 = Cube {terms = [Zero, Zero, Zero, One], noOfTerms = 4, minTerms = [1]}
                let cube2 = Cube {terms = [Zero, Zero, Zero, Zero], noOfTerms = 4, minTerms = [0]}
                canJoinCube cube1 cube2 `shouldBe` True
        describe "cannot join with no DC's" $ do
            it "should work when two positions differ" $ do
                let cube1 = Cube {terms = [Zero, Zero, Zero, One], noOfTerms = 4, minTerms = [1]}
                let cube2 = Cube {terms = [Zero, One, Zero, Zero], noOfTerms = 4, minTerms = [4]}
                canJoinCube cube1 cube2 `shouldBe` False
            it "should work when three positions differ" $ do
                let cube1 = Cube {terms = [Zero, Zero, Zero, One], noOfTerms = 4, minTerms = [1]}
                let cube2 = Cube {terms = [One, One, Zero, Zero], noOfTerms = 4, minTerms = [12]}
                canJoinCube cube1 cube2 `shouldBe` False
        describe "can join with DC's" $ do
            it "should work when DC position differs" $ do
                let cube1 = Cube {terms = [Zero, DC, Zero, One], noOfTerms = 4, minTerms = [1]}
                let cube2 = Cube {terms = [Zero, DC, One, One], noOfTerms = 4, minTerms = [3]}
                canJoinCube cube1 cube2`shouldBe` True
            it "should work when first position differs" $ do
                let cube1 = Cube {terms = [Zero, Zero, Zero, DC], noOfTerms = 4, minTerms = [1]}
                let cube2 = Cube {terms = [One, Zero, Zero, DC], noOfTerms = 4, minTerms = [9]}
                canJoinCube cube1 cube2 `shouldBe` True
            it "should work when last position differs" $ do
                let cube1 = Cube {terms = [Zero, DC, DC, One], noOfTerms = 4, minTerms = [1]}
                let cube2 = Cube {terms = [Zero, DC, DC, Zero], noOfTerms = 4, minTerms = [0]}
                canJoinCube cube1 cube2 `shouldBe` True
        describe "cannot join with DC's" $ do
            it "should work when two positions differ" $ do
                let cube1 = Cube {terms = [Zero, Zero, Zero, DC], noOfTerms = 4, minTerms = [1]}
                let cube2 = Cube {terms = [Zero, One, Zero, Zero], noOfTerms = 4, minTerms = [4]}
                canJoinCube cube1 cube2 `shouldBe` False
            it "should work when three positions differ" $ do
                let cube1 = Cube {terms = [Zero, DC, Zero, DC], noOfTerms = 4, minTerms = [1]}
                let cube2 = Cube {terms = [One, One, Zero, Zero], noOfTerms = 4, minTerms = [12]}
                canJoinCube cube1 cube2 `shouldBe` False

