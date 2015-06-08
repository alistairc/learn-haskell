import Data.List
import Data.Char

wordChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['-','\'']

wordFilter c = elem c wordChars
 
tokenize :: [Char] -> [[Char]]
tokenize text = map (filter wordFilter) $ 
    words $ 
    map toLower text 

count :: [[Char]] -> [([Char],Int)]
count list = do
    let sorted = sortBy compare list
    let groups = groupBy (==) sorted
    let groupTotal grp = (head grp, length grp)
    map groupTotal groups

wordCount :: [Char] -> [([Char],Int)]
wordCount text = count $ tokenize text

main = print $ wordCount "This is some plain text.  This program will count the \
   \number of occurrences of each word and write to the console"

    
