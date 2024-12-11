module MyLib where

import Control.Arrow ((***))
import Control.Monad (join)
import Data.List (group, sort, transpose)
import Data.Maybe (fromJust)

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair = join (***)

-- Tries to convert a list of 2 element into a pair
toPair :: [a] -> Maybe (a, a)
toPair [x, y] = Just (x, y)
toPair _ = Nothing

-- Gets the first two elements of a list as a pair
firstTwo :: [a] -> Maybe (a, a)
firstTwo (x : y : _) = Just (x, y)
firstTwo _ = Nothing

-- Read string containing lines of white space separated values as a matrix
readMatrix :: (Read a) => String -> [[a]]
readMatrix = map (map read . words) . lines

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
listRemover =
    let
        helper :: [[a]] -> [a] -> [a] -> [[a]]
        helper acc _ [] = acc
        helper acc before (x : xs) = helper ((reverse before ++ xs) : acc) (x : before) xs
     in
        helper [] []

-- Get the `n`-th main diagonal of the matrix considering periodicity
mainDiagonal :: Int -> [[a]] -> [a]
mainDiagonal n matrix = zipWith (!!) (map cycle matrix) [n ..]

-- Get the all the main diagonals of the matrix considering periodicity
mainDiagonals :: [[a]] -> [[a]]
mainDiagonals matrix =
    let ncols = length $ head matrix
     in map (`mainDiagonal` matrix) [0 .. ncols - 1]

-- Get all views of a matrix: rows, columns, main diagonals and secondary diagonals. Considering periodicity
allViews :: [[a]] -> [[[a]]]
allViews matrix = map ($ matrix) [id, transpose, mainDiagonals, secondaryDiagonals]
  where
    secondaryDiagonals :: [[a]] -> [[a]]
    secondaryDiagonals = mainDiagonals . map reverse

-- Get the `n`-th main diagonal of the matrix disregarding periodicity
mainDiagonal' :: Int -> [[a]] -> [a]
mainDiagonal' n matrix =
    let ncols = length $ head matrix
     in map (uncurry (!!)) $ filter ((>= 0) . snd) $ zip matrix [n .. ncols - 1]

-- Get the all the main diagonals of the matrix disregarding periodicity
mainDiagonals' :: [[a]] -> [[a]]
mainDiagonals' matrix =
    let ncols = length $ head matrix
     in map (`mainDiagonal'` matrix) [-ncols + 1 .. ncols - 1]

secondaryDiagonals' :: [[a]] -> [[a]]
secondaryDiagonals' = mainDiagonals' . map reverse

-- Get all views of a matrix: rows, columns, main diagonals and secondary diagonals. Disregarding periodicity
allViews' :: [[a]] -> [[[a]]]
allViews' matrix = map ($ matrix) [id, transpose, mainDiagonals', secondaryDiagonals]
  where
    secondaryDiagonals :: [[a]] -> [[a]]
    secondaryDiagonals = mainDiagonals' . map reverse
