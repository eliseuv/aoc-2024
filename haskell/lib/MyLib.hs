module MyLib where

import Data.List (group, sort, transpose)
import Data.Maybe (fromJust)

-- Read string containing lines of white space separated values as a matrix
readMatrix :: (Read a) => String -> [[a]]
readMatrix = map (map read . words) . lines

-- Gets the first two elements of a list as a pair
firstTwo :: [a] -> Maybe (a, a)
firstTwo (x : y : _) = Just (x, y)
firstTwo _ = Nothing

-- Tries to parse a text containing two white space separated columns into a pair of lists
read2Cols :: (Read a) => String -> ([a], [a])
read2Cols = fromJust . firstTwo . transpose . readMatrix

-- Get the number of occurrences of a given element in a list
countOccurences :: (Eq a) => a -> [a] -> Int
countOccurences x = length . filter (== x)

-- Counts the number of occurrences of each unique element in a list
getCounts :: (Ord a) => [a] -> [(a, Int)]
getCounts = map (\lst -> (head lst, length lst)) . group . sort
