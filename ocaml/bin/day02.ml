open Aoc2024.Utils

let () = print_endline "Advent of Code 2024 day 2"

let check_safety report =
  let diff = list_diff report in
  List.for_all
    (fun dx ->
      let abs_dx = abs dx in
      1 <= abs_dx && abs_dx <= 3 )
    diff
  &&
  let diff_signs = List.map sign diff in
  list_all_equal diff_signs

let check_safety_dampened report =
  let dampened_candidates = list_remover report in
  list_any (List.map check_safety dampened_candidates)

let part1 reports = count_true (List.map check_safety reports)

let part2 reports =
  count_true
    (List.map
       (fun report -> check_safety report || check_safety_dampened report)
       reports )

let solve input_filename =
  let reports = load_rows input_filename in
  Stdio.printf "Part 1: Number of safe reports = %d\n" @@ part1 reports ;
  Stdio.printf "Part 2: Number of safe reports considering dampening = %d\n"
  @@ part2 reports

let () = solve "../inputs/day-02.txt"
