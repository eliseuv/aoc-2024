module Day03 where

import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import MyLib (toPair)
import Text.Printf (printf)
import Text.Regex.PCRE

-- Parse all the instructions as pairs of instruction name and arguments list
parseInstructions :: String -> [(String, [Int])]
parseInstructions program = mapMaybe tryParse matches
  where
    matches :: [[String]]
    matches = program =~ "(\\w+)\\(([\\d\\.,]+)\\)"
    tryParse :: [String] -> Maybe (String, [Int])
    tryParse [_, inst, args] = Just (inst, parseArgs args)
    tryParse _ = Nothing
    parseArgs :: String -> [Int]
    parseArgs = map read . splitOn ","

-- Filter out the parts of the program after the `don't` instruction keeping those after the `do` instructions
filterDon'ts :: String -> String
filterDon'ts = concatMap (head . splitOn "don't()") . splitOn "do()"

part1 :: String -> Int
part1 =
    sum
        . map (uncurry (*)) -- Perform multiplication on argument pairs
        . mapMaybe (toPair . snd) -- Get the two argument as a pair
        . filter (\(instruction, _) -> instruction =~ "mul$") -- Filter the `mul` instructions
        . parseInstructions

part2 :: String -> Int
part2 = part1 . filterDon'ts

inputPath :: FilePath
inputPath = "../inputs/day-03.txt"

solution :: IO ()
solution =
    do
        putStrLn "Day 3"
        program <- readFile inputPath
        printf "Part 1: Sum of all `mul` instructions = %d\n" $ part1 program
        printf "Part 2: Sum of all `mul` instructions considering `do`s and `don't`s = %d\n" $ part2 program
