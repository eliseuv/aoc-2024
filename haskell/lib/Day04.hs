module Day04 where

import Control.Applicative (liftA2)
import Control.Monad (join, liftM2)
import Foreign (fromBool)
import MyLib (allViews', mapPair)
import Text.Printf (printf)
import Text.Regex.PCRE ((=~))

-- Count the number of occurrences of a given word on a line
countWord :: String -> String -> Int
countWord word line = line =~ word

-- Count the number of occurrences of a given word (regular or reversed) on a line
countWord' :: String -> String -> Int
countWord' word = liftA2 (+) (countWord word) (countWord (reverse word))

-- Get the two diagonals of the X shape with given top left coordinates of a matrix
getXElements :: [[a]] -> Int -> (Int, Int) -> ([a], [a])
getXElements matrix size (row_start, col_start) =
    let
        idx_range = [row_start .. row_start + size - 1]
        idx_se = zip idx_range [col_start ..]
        idx_ne = zip (reverse idx_range) [col_start ..]
     in
        mapPair (map (getIndex matrix)) (idx_se, idx_ne)
  where
    getIndex :: [[a]] -> (Int, Int) -> a
    getIndex mat (i, j) = mat !! i !! j

part1 :: [String] -> Int
part1 = sum . map (countWord' "XMAS") . join . allViews'

part2 :: [String] -> Int
part2 grid =
    let
        size = 3
        nrows = length grid
        ncols = length $ head grid
        idx_pairs = liftM2 (,) [0 .. nrows - size] [0 .. ncols - size]
     in
        sum $ map (fromBool . uncurry (&&) . mapPair isMatch . getXElements grid size) idx_pairs
  where
    isMatch :: String -> Bool
    isMatch str =
        let pattern = "MAS"
         in (str =~ pattern) || (str =~ reverse pattern)

inputPath :: FilePath
inputPath = "../inputs/day-04.txt"

solution :: IO ()
solution =
    do
        putStrLn "Day 4"
        input <- readFile inputPath
        let grid = lines input
        printf "Part 1: Number of occurences of \"XMAS\" = %d\n" $ part1 grid
        printf "Part 2: Number of occurences of X-\"MAS\" = %d\n" $ part2 grid
