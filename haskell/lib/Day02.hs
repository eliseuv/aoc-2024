module Day02 where

import Foreign (fromBool)
import MyLib (allEqual, diff, inRange, listRemover, readMatrix)
import Text.Printf (printf)

isSafe :: (Ord a, Num a) => [a] -> Bool
isSafe report =
    let dx_report = diff report
     in and (allEqual (map signum dx_report) : map (inRange (1, 3) . abs) dx_report)

isSafeDampened :: (Ord a, Num a) => [a] -> Bool
isSafeDampened report = or (isSafe report : map isSafe dampenedCandidates)
  where
    dampenedCandidates = listRemover report

part1 :: (Ord a, Num a) => [[a]] -> Int
part1 = sum . map (fromBool . isSafe)

part2 :: (Ord a, Num a) => [[a]] -> Int
part2 = sum . map (fromBool . isSafeDampened)

inputPath :: FilePath
inputPath = "../inputs/day-02.txt"

solution :: IO ()
solution =
    do
        putStrLn "Day 2"
        contents <- readFile inputPath
        let reports = readMatrix contents :: [[Int]]
        printf "Part 1: Number of safe reports = %d\n" $ part1 reports
        printf "Part 2: Number of safe reports considering dampening = %d\n" $ part2 reports
