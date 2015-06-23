import Part2
import Test.Hspec
import Data.List
import qualified Data.Text.Lazy as T

shouldBeEquivalent x y = sortBy compare x `shouldBe` sortBy compare y

main = hspec $ do
    describe "calculating the lowestGoalDifference" $ do
        it "should be null for empty file" $ do
            lowestGoalDifference [] `shouldBe` Nothing
        it "should be the only date for a single line" $ do
            lowestGoalDifference [Score "Team 1" 12 8] `shouldBe` Just "Team 1"
        it "should be the day number with the the lowest spread" $ do
            lowestGoalDifference [
                    Score "Team 1" 12 8,
                    Score "Team 2" 10 9,
                    Score "Team 3" 13 10 
                ] `shouldBe` Just "Team 2"
            
    describe "reading the file" $ do
        it "should handle empty files gracefully" $ do
            readScores (T.pack "") `shouldBe` []
            
        it "should read rows from the file" $ do
            readScores (T.pack "    1. Arsenal         38    26   9   3    79  -  36    87\n\
                               \    2. Liverpool       38    24   8   6    67  -  30    80\n")
                `shouldBe` [Score "Arsenal" 79 36, Score "Liverpool" 67 30]
                
        it "should skip the header, footer and blank rows" $ do
            readScores (T.pack "       Team            P     W    L   D    F      A     Pts\n\
                               \\n\
                               \    1. Arsenal         38    26   9   3    79  -  36    87\n\
                               \   -------------------------------------------------------\n")
                `shouldBe` [Score "Arsenal" 79 36]
            
    describe "integration: file -> lowestGoalDifference" $ do
        it "success" $ do
            lowestGoalDifferenceInFile (T.pack
                    "    1. Arsenal         38    26   9   3    79  -  36    87\n\
                    \    2. Liverpool       38    24   8   6    67  -  30    80\n")
                `shouldBe` Just "Liverpool"
        it "empty file" $ do
            lowestGoalDifferenceInFile (T.pack "")
                `shouldBe` Nothing
