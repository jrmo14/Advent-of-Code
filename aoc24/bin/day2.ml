open Printf

let parse input =
  List.map
    (fun line -> String.split_on_char ' ' line |> List.map int_of_string)
    input

let part1 input =
  let disc pairs =
    (Util.all (fun (x, y) -> x > y) pairs
    || Util.all (fun (x, y) -> x < y) pairs)
    && Util.all (fun (x, y) -> 3 >= Int.abs (x - y)) pairs
  in
  Some
    (List.map (fun line -> if Util.pairs line |> disc then 1 else 0) input
    |> List.fold_left ( + ) 0)

(*
   TIL about infix operators...
   infix range operator from stack overflow
   https://stackoverflow.com/questions/243864/what-is-the-ocaml-idiom-equivalent-to-pythons-range-function
*)
let ( -- ) i j =
  let rec aux n acc = if n < i then acc else aux (n - 1) (n :: acc) in
  aux j []

let part2 input =
  let disc pairs =
    (Util.all (fun (x, y) -> x > y) pairs
    || Util.all (fun (x, y) -> x < y) pairs)
    && Util.all (fun (x, y) -> 3 >= Int.abs (x - y)) pairs
  in
  Some
    (List.map
       (fun line ->
         (* baseline check... all data are good *)
         if Util.pairs line |> disc then 1
         else if
           (* check if we can make the data good by dropping a single item *)
           Util.any
             (fun idx ->
               Util.pairs (Util.take idx line @ Util.skip (idx + 1) line)
               |> disc)
             (0 -- List.length line)
         then 1
         else 0)
       input
    |> List.fold_left ( + ) 0)

let run =
  let input = Util.read_lines "res/day2" |> parse in
  printf "Day 2\n";
  (match part1 input with
  | Some ans -> printf "Part 1: %d\n" ans
  | None -> print_endline "Part 1 is None");
  match part2 input with
  | Some ans -> printf "Part 1: %d\n" ans
  | None -> print_endline "Part 1 is None"
