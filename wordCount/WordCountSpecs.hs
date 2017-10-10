import WordCount hiding (main)
import Test.Hspec
import Data.List

shouldBeEquivalent x y = sortBy compare x `shouldBe` sortBy compare y

main = hspec $ do
    describe "word count" $ do
        it "should be empty when no input" $ do
            wordCount "" `shouldBe` []
            
        it "should count words" $ do
            wordCount "single triple double triple double triple" `shouldBeEquivalent` [
                    ("single", 1),
                    ("double", 2),
                    ("triple", 3)
                ]
                
        it "should sort the output" $ do
            map fst (wordCount "banana apple carrot") `shouldBe` ["apple", "banana", "carrot"]
            
            
        it "should ignore punctuation" $ do
            wordCount "hello, hello hello. hello? hello!" `shouldBe` [("hello",5)]
            
        it "should treat all words as lowercase" $ do
            wordCount "hello Hello HELLO hellO" `shouldBe` [("hello",4)]
