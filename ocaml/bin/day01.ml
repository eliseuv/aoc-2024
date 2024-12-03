open Base

let part1 lines =


let solve filename =
    let lines = Aoc2024.Utils.read_lines filename in
    Stdio.printf "Part 1: %d\n" @@ part1 lines
       (* Stdio.printf "Part 2: %d\n" @@ part2 lines;; *)


let () =
    solve "../inpts/day-01.txt"
