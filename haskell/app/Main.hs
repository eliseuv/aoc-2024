module Main where

import Day01 qualified
import Day02 qualified
import Day03 qualified
import Day04 qualified
import Day05 qualified
import System.Environment (getArgs)

main :: IO ()
main = do
    putStrLn "Advent of Code 2024"
    args <- getArgs
    case args of
        "01" : _ -> Day01.solution
        "02" : _ -> Day02.solution
        "03" : _ -> Day03.solution
        "04" : _ -> Day04.solution
        "05" : _ -> Day05.solution
        _ -> error "None or invalid day number provided."
