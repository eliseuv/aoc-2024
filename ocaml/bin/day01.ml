let () = print_endline "Advent of Code 2024 day 1"

let part1 (list_left, list_right) =
  let sorted_left = List.sort compare list_left in
  let sorted_right = List.sort compare list_right in
  List.fold_left
    (fun sum x -> sum + x)
    0
    (List.map
       (fun (x, y) -> abs (x - y))
       (List.combine sorted_left sorted_right) )

let part2 (list_left, list_right) =
  let occur_left = Aoc2024.Utils.count_occurences list_left in
  List.fold_left
    (fun sum (value, count) ->
      sum
      + (count * value * Aoc2024.Utils.count_value_occurences value list_right)
      )
    0 occur_left

let solve input_filename =
  let lists = List.split (Aoc2024.Utils.read_list_of_pairs input_filename) in
  Stdio.printf "Part 1: Total distance = %d\n" @@ part1 lists ;
  Stdio.printf "Part 2: Similarity score = %d\n" @@ part2 lists

let () = solve "../inputs/day-01.txt"
