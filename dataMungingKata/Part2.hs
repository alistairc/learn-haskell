module Part2 (
        Score (..), 
        lowestGoalDifference, 
        readScores, 
        lowestGoalDifferenceInFile
    ) where

import Common
import qualified Data.Text.Lazy as T
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy.Encoding as T

import Data.List
import Data.Maybe
import Text.Read
import System.IO
import Text.Regex.Posix

data Score = Score {
        teamName :: String,
        goalsFor :: Int,
        goalsAgainst :: Int
    } deriving (Show, Eq)

goalDifference x = goalsFor x - goalsAgainst x

lowestGoalDifference :: [Score] -> Maybe String
lowestGoalDifference scores = maybeSelect teamName (lowest goalDifference scores)

toScore :: [String] -> Maybe Score
toScore (posCol:teamCol:_:_:_:_:forCol:_:againstCol:_)
    | isDataRow = Just $ Score { 
            teamName = teamCol, 
            goalsFor = read $ forCol, 
            goalsAgainst = read $ againstCol 
        }
    | otherwise = Nothing
    where 
        isDataRow = posCol =~ "[[:digit:]]+\\."
toScore _ = Nothing
    
readScores :: T.Text -> [Score]
readScores = readWordDelimitedText toScore
    
lowestGoalDifferenceInFile :: T.Text -> Maybe String
lowestGoalDifferenceInFile = processFile lowestGoalDifference readScores

main = do
    file <- openFile "./football.dat" ReadMode
    content <- B.hGetContents file
    print (lowestGoalDifferenceInFile (T.decodeUtf8 content))
    hClose file
