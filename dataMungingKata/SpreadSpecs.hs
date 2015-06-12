import Spread
import Test.Hspec
import Data.List

shouldBeEquivalent x y = sortBy compare x `shouldBe` sortBy compare y

main = hspec $ do

    describe "calculating the lowestSpread" $ do
        it "should be null for empty file" $ do
            lowestSpread [] `shouldBe` Nothing
        it "should be the only date for a single line" $ do
            lowestSpread [Weather 1 12 8] `shouldBe` Just 1
        it "should be the day number with the the lowest spread" $ do
            lowestSpread [
                    Weather 1 12 8,
                    Weather 2 10 9,
                    Weather 3 13 10 
                ] `shouldBe` Just 2
            
    describe "reading the file" $ do
        it "should handle empty files gracefully" $ do
            readWeather "" `shouldBe` []
            
        it "should read rows from the file" $ do
            readWeather "   1  88    59    74          53.8       0.00 F       280  9.6 270  17  1.6  93 23 1004.5\n\
                        \   2  79    63    71          46.5       0.00         330  8.7 340  23  3.3  70 28 1004.5\n"
                `shouldBe` [Weather 1 88 59, Weather 2 79 63]
                
        it "should skip the header, footer and blank rows" $ do
            readWeather "  Dy MxT   MnT   AvT   HDDay  AvDP 1HrP TPcpn WxType PDir AvSp Dir MxS SkyC MxR MnR AvSLP\n\
                        \\n\
                        \   1  88    59    74          53.8       0.00 F       280  9.6 270  17  1.6  93 23 1004.5\n\
                        \\n\
                        \  mo  82.9  60.5  71.7    16  58.8       0.00              6.9          5.3\n"

                `shouldBe` [Weather 1 88 59]
           
        it "should ignore stars after the temperature numbers" $ do
            readWeather "   1  88*   59    74          53.8       0.00 F       280  9.6 270  17  1.6  93 23 1004.5\n\
                        \   2  79    63*   71          46.5       0.00         330  8.7 340  23  3.3  70 28 1004.5\n"
                `shouldBe` [Weather 1 88 59, Weather 2 79 63]
            
    describe "integration: file -> lowestSpread" $ do
        it "success" $ do
            lowestSpreadInFile
                    "   1  88    59    74          53.8       0.00 F       280  9.6 270  17  1.6  93 23 1004.5\n\
                    \   2  79    63    71          46.5       0.00         330  8.7 340  23  3.3  70 28 1004.5\n"
                `shouldBe` Just 2
        it "empty file" $ do
            lowestSpreadInFile ""
                `shouldBe` Nothing
            

