(* Read list of integer pairs separated by whitespace from datafile *)
let read_list_of_pairs filename =
  let ic = Stdio.In_channel.create filename in
  let rec read_lines acc_list =
    try
      let line = input_line ic in
      let parts = Str.split (Str.regexp "[ \t]+") line in
      match parts with
      | [a; b] ->
          read_lines ((int_of_string a, int_of_string b) :: acc_list)
      | _ ->
          read_lines acc_list
    with End_of_file -> Stdio.In_channel.close ic ; List.rev acc_list
  in
  read_lines []

(* Count the number of occurences of a given value in a list *)
let count_value_occurences value list =
  List.fold_left (fun acc x -> acc + if x == value then 1 else 0) 0 list

(* Returns a list of pairs containing at the first position a value and at the second the number of times this values occurs in the given list *)
let count_occurences list =
  let uniq = List.sort_uniq compare list in
  let counts = List.map (fun value -> count_value_occurences value list) uniq in
  List.combine uniq counts
