import Test.Hspec
import Data.List (sortBy)

myLength :: [a] -> Int
myLength [] = 0
myLength (x:[]) = 1
myLength (x:xs) = 1 + myLength xs

mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x:[]) = x
mySum (x:xs) = x + mySum xs

mean :: [Double] -> Double
mean [] = 0
mean x = mySum x / fromIntegral (myLength x)

isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome x = x == reverse x

palindrome:: Eq a => [a] -> [a]
palindrome [] = []
palindrome x 
    | isPalindrome x = x
    | otherwise = x ++ tail (reverse x)
    
lengthSort :: [[a]] -> [[a]]
lengthSort x = sortBy lengthCompare x
    where lengthCompare a b = compare (length a) (length b)
    
intersperse :: a -> [[a]] -> [a]
intersperse _ [] = []
intersperse _ [single] = single
intersperse separator (item:rest) = item ++ [separator] ++ (intersperse separator rest)



data Tree = 
    Node { left :: Tree, right :: Tree } 
    | Empty
    
height :: Tree -> Int
height Empty = 0
height (Node left right) = 1 + max (height left) (height right)

main = hspec $ do
    describe "myLength" $ do
        it "should behave the same as the built in function" $ do
            myLength [] `shouldBe` length []
            myLength ["one"] `shouldBe` length ["one"]
            myLength ["one","two","three"] `shouldBe` length ["one","two","three"]
            
    describe "mean" $ do
        it "should calculate means" $ do
            mean [] `shouldBe` 0
            mean [5] `shouldBe` 5
            mean [1,1.6,4] `shouldBe` 6.6/3
            
    describe "palindrome" $ do
        it "should produce palindromes" $ do
            palindrome [] `shouldBe` empty
            palindrome ["first"] `shouldBe` ["first"]
            palindrome [1,2,3] `shouldBe` [1,2,3,2,1]
        it "should leave existing palindromes alone" $ do
            palindrome [1,2,3,2,1] `shouldBe` [1,2,3,2,1]
            
        it "should detect palindromes" $ do
            isPalindrome empty `shouldBe` True
            isPalindrome [1] `shouldBe` True
            isPalindrome [1,2,3,3,2,1] `shouldBe` True
            isPalindrome [1,2,3,2,1] `shouldBe` True
            isPalindrome "abcba" `shouldBe` True
            
            isPalindrome [1,2,3] `shouldBe` False
            isPalindrome "baa" `shouldBe` False
            
    describe "lengthSort" $ do
        it "should sort lists of lists by length" $ do
            lengthSort [] `shouldBe` ([]::[[()]])
            lengthSort [[]] `shouldBe` ([[]]::[[()]])
            lengthSort [[1],[2]] `shouldBe` [[1],[2]]
            lengthSort [[1,2],[3,4,5],[6]] `shouldBe` [[6],[1,2],[3,4,5]]
            lengthSort ["first","second","third"] `shouldBe` ["first","third","second"]
            
    describe "intersperse" $ do
        it "should add a separator between elements" $ do
            intersperse ',' [] `shouldBe` ""
            intersperse ',' ["foo"] `shouldBe` "foo"
            intersperse ',' ["foo","bar","baz","quux"] `shouldBe` "foo,bar,baz,quux"
    
    describe "height" $ do
        it "should return the height of symmetrical trees" $ do
            height Empty `shouldBe` 0
            height Node { left = Empty, right = Empty } `shouldBe` 1
            height Node { 
                left = Node { left = Empty, right = Empty }, 
                right = Node { left = Empty, right = Empty }
            }  `shouldBe` 2
        it "should return the longest height of asymmetrical trees" $ do
            height Node { 
                left = Node { 
                    left = Empty,
                    right = Node { left = Empty, right = Empty }
                },
                right = Node { left = Empty, right = Empty }
            } `shouldBe` 3
            
            
    where
        empty = ([]::[()])  -- "shouldBe" needs a type deriving Show, Eq; so just [] won't do
    
            
            
            
        