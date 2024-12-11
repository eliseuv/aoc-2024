module Day05 where

import Control.Monad (join)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Text.Regex.PCRE ((=~))

splitAtFirstOccurence :: (Eq a) => a -> [a] -> ([a], [a])
splitAtFirstOccurence =
    let
        splitter past _ [] = (reverse past, [])
        splitter past value (x : xs) =
            if x == value
                then (reverse past, xs)
                else splitter (x : past) value xs
     in
        splitter []

parseRule :: String -> Maybe (Int, Int)
parseRule str =
    case join (str =~ "(\\d+)\\|(\\d+)" :: [[String]]) of
        [_, x, y] -> Just (read x, read y)
        _ -> Nothing

parseUpdate :: String -> [Int]
parseUpdate = map read . splitOn ","

parseInput :: String -> ([(Int, Int)], [[Int]])
parseInput input =
    let (rules, updates) = splitAtFirstOccurence "" $ lines input
     in (mapMaybe parseRule rules, map parseUpdate updates)

inputPath :: FilePath
inputPath = "../inputs/day-05-test.txt"

solution :: IO ()
solution =
    do
        putStrLn "Day 5"
        input <- readFile inputPath
        let (rules, updates) = parseInput input
        print rules
        print updates
