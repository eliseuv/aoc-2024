module Day02 where

import MyLib (readMatrix)

-- isSafe report =
--     let dx = diff report
--      in dx

inputPath :: FilePath
inputPath = "../inputs/day-02-test.txt"

solution :: IO ()
solution =
    do
        putStrLn "Day 2"
        contents <- readFile inputPath
        let reports = readMatrix contents :: [[Int]]
        print reports
