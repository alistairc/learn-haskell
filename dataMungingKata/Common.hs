module Common (
        processFile,
        readWordDelimitedText,
        lowest,
        maybeSelect
    ) where

import qualified Data.Text.Lazy as T
import Data.Maybe
import Data.List

processFile :: ([a] -> Maybe b) -> (T.Text -> [a]) -> T.Text -> Maybe b
processFile selectResult toRecords content = (selectResult . toRecords) content

readWordDelimitedText :: ([String] -> Maybe a) -> T.Text -> [a]
readWordDelimitedText mapper content =
    map fromJust (filter isJust (map (mapper . words . T.unpack) (T.lines content)))

lowest :: Ord b => (a -> b) -> [a] -> Maybe a
lowest _ [] = Nothing
lowest getOrder list = Just $ head (sortBy diff list)
    where diff x y = compare (getOrder x) (getOrder y)

maybeSelect :: (a -> b) -> Maybe a -> Maybe b
maybeSelect toResult thing 
    | isNothing thing = Nothing
    | otherwise = Just $ toResult (fromJust thing)
