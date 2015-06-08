module WordCount where

import Data.List
import Data.Char

toLowerString :: String -> String
toLowerString s = map toLower s
 
tokenize :: String -> [String]
tokenize text = 
    (words . toLowerString . wordCharsOnly) text
    where 
        wordCharsOnly s = filter wordFilter s
        wordFilter c = elem c wordChars
        wordChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['-','\''] ++ [' ','\n','\t']

count :: [String] -> [(String,Int)]
count list = map countGroup $ (group . sort) list
    where
        sort = sortBy compare
        group = groupBy (==)
        countGroup grp = (head grp, length grp)

wordCount :: String -> [(String,Int)]
wordCount text = (count . tokenize) text

main = print $ wordCount "This is some plain text.\nThis program will count the \
   \number of occurrences of each word and write to the console"

    
