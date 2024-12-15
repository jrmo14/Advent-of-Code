open Printf

let parse input =
  List.map
    (fun line ->
      String.split_on_char ' ' line |> fun e ->
      (int_of_string (List.hd e), int_of_string (Util.last_itm e)))
    input

let part1 input =
  let left, right = input in
  let left = List.sort ( - ) left in
  let right = List.sort ( - ) right in
  Some
    (List.fold_left ( + ) 0
       (Util.zip left right |> List.map (fun (l, r) -> Int.abs (l - r))))

let part2 input =
  let left, right = input in
  let right_counts = Util.counter right in
  Some
    (List.map
       (fun e ->
         match Hashtbl.find_opt right_counts e with
         | Some v -> v * e
         | None -> 0)
       left
    |> List.fold_left ( + ) 0)

let run () =
  let input = Util.unzip (Util.read_lines "res/day1" |> parse) in
  printf "Day 1\n";
  (match part1 input with
  | Some ans -> printf "Part 1: %d\n" ans
  | None -> print_endline "Part 1 is None");
  match part2 input with
  | Some ans -> printf "Part 2: %d\n" ans
  | None -> print_endline "Part 2 is None"
