(* Read list of integer pairs separated by whitespace from datafile *)
let load_2cols filename =
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

(* Load each line of space separated values of a text file as a list *)
let load_rows filename =
  let ic = Stdio.In_channel.create filename in
  let rec read_rows acc =
    let maybe_line = Stdio.In_channel.input_line ic in
    match maybe_line with
    | Some line ->
        read_rows
          (List.map int_of_string (Str.split (Str.regexp "[ \t]+") line) :: acc)
    | None ->
        Stdio.In_channel.close ic ; List.rev acc
  in
  read_rows []

let string_of_list list = String.concat " " (List.map string_of_int list)

let print_numbered string_of =
  List.iteri (fun i x -> Printf.printf "[%d] : %s\n" i (string_of x))

let sign n = compare n 0

(* Calculate the difference between consecutive values in a list, similar to `numpy.diff` *)
let rec list_diff = function
  | [] | [_] ->
      []
  | y :: x :: xs ->
      (x - y) :: list_diff (x :: xs)

(* Checks if all the elements of a list are equal to each other *)
let rec list_all_equal = function
  | [] | [_] ->
      true
  | y :: x :: xs ->
      y = x && list_all_equal (x :: xs)

(* Check if any value in an array of bools is true *)
let rec list_any = function [] -> false | x :: xs -> x || list_any xs

(* Generate copies of a given list, each copy with an element removed *)
let list_remover list =
  let rec remover acc before = function
    | [] ->
        acc
    | x :: xs ->
        remover (List.concat [List.rev before; xs] :: acc) (x :: before) xs
  in
  remover [] [] list

(* Count the number of occurences of a given value in a list *)
let count_value_occurences value list =
  List.fold_left (fun acc x -> acc + if x = value then 1 else 0) 0 list

(* Returns a list of pairs containing at the first position a value and at the second the number of times this values occurs in the given list *)
let count_occurences list =
  let uniq = List.sort_uniq compare list in
  let counts = List.map (fun value -> count_value_occurences value list) uniq in
  List.combine uniq counts

(* Count the number of `true` values in a list of bools *)
let count_true =
  List.fold_left (fun sum safe -> if safe then sum + 1 else sum) 0
