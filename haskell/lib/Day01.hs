module Day01 where

import Data.Bifunctor (Bifunctor (bimap))
import Data.List (sort)
import MyLib (countOccurences, getCounts, read2Cols)
import Text.Printf (printf)

distance :: (Num a, Ord a) => [a] -> [a] -> a
distance x y =
    sum $ abs <$> zipWith subtract (sort x) (sort y)

distance' :: (Num a, Ord a) => [a] -> [a] -> a
distance' x y = sum $ map abs $ uncurry (zipWith subtract) $ curry (bimap sort sort) x y

similarityScore :: (Num a, Eq a, Ord a) => [a] -> [a] -> a
similarityScore x y =
    sum $ (\(value, count) -> fromIntegral (count * countOccurences value y) * value) <$> getCounts x

part1 :: [Int] -> [Int] -> Int
part1 = distance

part2 :: [Int] -> [Int] -> Int
part2 = similarityScore

inputPath :: FilePath
inputPath = "../inputs/day-01-test.txt"

solution :: IO ()
solution =
    do
        putStrLn "Day 1"
        contents <- readFile inputPath
        let (col_left, col_right) = read2Cols contents :: ([Int], [Int])
        printf "Part 1: Distance = %d\n" $ part1 col_left col_right
        printf "Part 2: Similarity = %d\n" $ part2 col_left col_right
        print $ distance' col_left col_right
