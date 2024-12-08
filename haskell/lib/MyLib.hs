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

-- Similar to `numpy.diff`
diff :: (Num a) => [a] -> [a]
diff =
    let
        helper :: (Num a) => [a] -> [a] -> [a]
        helper acc [] = acc
        helper acc [_] = acc
        helper acc (y : x : xs) = helper ((y - x) : acc) (x : xs)
     in
        helper []

-- Checks if all elements in a list are equal to each other
allEqual :: (Eq a) => [a] -> Bool
allEqual lst =
    all (== head lst) (tail lst)

-- Checks if all elements of a list are between a range given by a value pair
inRange :: (Ord a) => (a, a) -> a -> Bool
inRange (a, b) x =
    (a <= x) && (x <= b)

-- Generates copy of a given list each copy having a different element from the original list removed
listRemover :: [a] -> [[a]]
listRemover list =
    let
        remover acc _ [] = acc
        remover acc before (x : xs) = remover ((reverse before ++ xs) : acc) (x : before) xs
     in
        remover [] [] list
