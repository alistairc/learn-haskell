module Spread (
    Weather (..), 
    readWeather, 
    lowestSpread, 
    lowestSpreadInFile) where

import Data.List -- (sortBy)
import System.IO -- (hGetContents, hClose, openFile, IOMode(..))
import Data.Maybe -- (isNothing, isJust, fromJust)
import Text.Read -- (readMaybe)

data Weather = Weather {
        dayNum :: Int,
        maxTemp :: Int,
        minTemp :: Int
    } deriving (Show, Eq)

spread x = maxTemp x - minTemp x

lowestSpread :: [Weather] -> Maybe Int
lowestSpread [] = Nothing
lowestSpread weathers = Just $ dayNum (head (sortBy spreadDiff weathers))
    where spreadDiff x y = compare (spread x) (spread y)
    
toWeather :: [String] -> Maybe Weather
toWeather (first:second:third:_) 
    | isNothing maybeDay = Nothing
    | otherwise = Just $ Weather { 
            dayNum = fromJust maybeDay, 
            maxTemp = read $ clean second, 
            minTemp = read $ clean third 
        }
    where 
        maybeDay = readMaybe first
        clean = filter ((/=) '*')
toWeather _ = Nothing
    
readWeather :: String -> [Weather]
readWeather content =
    map fromJust (filter isJust (map (toWeather . words) (lines content)))
    
lowestSpreadInFile :: String -> Maybe Int
lowestSpreadInFile = (lowestSpread . readWeather)

main = do
    file <- openFile "./weather.dat" ReadMode
    content <- hGetContents file
    print (lowestSpreadInFile content)
    hClose file
