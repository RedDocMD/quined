import           Control.Exception (evaluate)
import           Data.List
import           Lib
import           Test.Hspec

main :: IO ()
main = hspec $ do
    describe "Cube creation from min-term" $ do
        describe "Zero" $ do
            it "should convert 0 properly" $ do
                unitCube 0 4 `shouldBe` Cube {terms = [Zero, Zero, Zero, Zero], noOfTerms = 4, minTerms = [0]}
        describe "Four" $ do
            it "should convert 4 properly in 3 terms" $ do
                unitCube 4 3 `shouldBe` Cube {terms = [One, Zero, Zero], noOfTerms = 3, minTerms = [4]}
            it "should convert 4 properly in 4 terms" $ do
                unitCube 4 4 `shouldBe` Cube {terms = [Zero, One, Zero, Zero], noOfTerms = 4, minTerms = [4]}
        describe "Five" $ do
            it "should convert 5 properly" $ do
                unitCube 5 4 `shouldBe` Cube {terms = [Zero, One, Zero, One], noOfTerms = 4, minTerms = [5]}
        describe "Eight" $ do
            it "should not convert 8 with 3 terms" $ do
                evaluate (unitCube 8 3) `shouldThrow` anyException
            it "should convert 8 with 4 terms" $ do
                unitCube 8 4 `shouldBe` Cube {terms = [One, Zero, Zero, Zero], noOfTerms = 4, minTerms = [8]}

    describe "Cube creation from terms" $ do
        it "should work with zero DC" $ do
            cubeFromTerms [One, Zero, One, Zero] `shouldBe` Cube {terms = [One, Zero, One, Zero], noOfTerms = 4, minTerms = [10]}
        it "should work with one DC" $ do
            cubeFromTerms [One, DC, One, Zero] `shouldBe` Cube {terms = [One, DC, One, Zero], noOfTerms = 4, minTerms = [10, 14]}
        it "should work with two DC" $ do
            cubeFromTerms [One, DC, One, DC] `shouldBe` Cube {terms = [One, DC, One, DC], noOfTerms = 4, minTerms = [10, 11, 14, 15]}

    describe "Can or Cannot join cubes" $ do
        describe "can join with no DC's" $ do
            it "should work when middle position differs" $ do
                let cube1 = cubeFromTerms [Zero, Zero, Zero, One]
                let cube2 = cubeFromTerms [Zero, Zero, One, One]
                canJoinCube cube1 cube2 `shouldBe` True
            it "should work when first position differs" $ do
                let cube1 = cubeFromTerms [Zero, Zero, Zero, One]
                let cube2 = cubeFromTerms [One, Zero, Zero, One]
                canJoinCube cube1 cube2 `shouldBe` True
            it "should work when last position differs" $ do
                let cube1 = cubeFromTerms [Zero, Zero, Zero, One]
                let cube2 = cubeFromTerms [Zero, Zero, Zero, Zero]
                canJoinCube cube1 cube2 `shouldBe` True
        describe "cannot join with no DC's" $ do
            it "should work when two positions differ" $ do
                let cube1 = cubeFromTerms [Zero, Zero, Zero, One]
                let cube2 = cubeFromTerms [Zero, One, Zero, Zero]
                canJoinCube cube1 cube2 `shouldBe` False
            it "should work when three positions differ" $ do
                let cube1 = cubeFromTerms [Zero, Zero, Zero, One]
                let cube2 = cubeFromTerms [One, One, Zero, Zero]
                canJoinCube cube1 cube2 `shouldBe` False
        describe "can join with DC's" $ do
            it "should work when DC position differs" $ do
                let cube1 = cubeFromTerms [Zero, DC, Zero, One]
                let cube2 = cubeFromTerms [Zero, DC, One, One]
                canJoinCube cube1 cube2 `shouldBe` True
            it "should work when first position differs" $ do
                let cube1 = cubeFromTerms [Zero, Zero, Zero, DC]
                let cube2 = cubeFromTerms [One, Zero, Zero, DC]
                canJoinCube cube1 cube2 `shouldBe` True
            it "should work when last position differs" $ do
                let cube1 = cubeFromTerms [Zero, DC, DC, One]
                let cube2 = cubeFromTerms [Zero, DC, DC, Zero]
                canJoinCube cube1 cube2 `shouldBe` True
        describe "cannot join with DC's" $ do
            it "should work when two positions differ" $ do
                let cube1 = cubeFromTerms [Zero, Zero, Zero, DC]
                let cube2 = cubeFromTerms [Zero, One, Zero, Zero]
                canJoinCube cube1 cube2 `shouldBe` False
            it "should work when three positions differ" $ do
                let cube1 = cubeFromTerms [Zero, DC, Zero, DC]
                let cube2 = cubeFromTerms [One, One, Zero, Zero]
                canJoinCube cube1 cube2 `shouldBe` False

    describe "Join cubes" $ do
        it "should join with 0 DC's" $ do
            let cube1 = cubeFromTerms [Zero, One, One, One]
            let cube2 = cubeFromTerms [Zero, Zero, One, One]
            joinCube cube1 cube2 `shouldBe` cubeFromTerms [Zero, DC, One, One]
        it "should join with 1 DC's" $ do
            let cube1 = cubeFromTerms [DC, One, One, One]
            let cube2 = cubeFromTerms [DC, Zero, One, One]
            joinCube cube1 cube2 `shouldBe` cubeFromTerms [DC, DC, One, One]
        it "should join with 2 DC's" $ do
            let cube1 = cubeFromTerms [DC, One, One, DC]
            let cube2 = cubeFromTerms [DC, Zero, One, DC]
            joinCube cube1 cube2 `shouldBe` cubeFromTerms [DC, DC, One, DC]

