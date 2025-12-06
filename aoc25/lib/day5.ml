open Printf
open Util

let parse_range line =
  match String.split_on_char '-' line with
  | [ lo; hi ] -> (int_of_string lo, int_of_string hi)
  | _ -> raise (Invalid_argument "uh oh")

let parse input =
  let rec inner ranges ingredients handle_ranges = function
    | line :: rem ->
        if 0 == String.length line then inner ranges ingredients false rem
        else if handle_ranges then
          inner (parse_range line :: ranges) ingredients handle_ranges rem
        else inner ranges (int_of_string line :: ingredients) handle_ranges rem
    | [] -> (ranges, ingredients)
  in
  inner [] [] true input

type range_tree = Leaf | Node of node
and node = { lo : int; hi : int; left : range_tree; right : range_tree }

let part1 (ranges, ingredients) =
  (* Computers are fast, just be dumb and check it all *)
  Some
    (List.fold_left
       (fun acc el ->
         acc
         + if any (fun (lo, hi) -> lo <= el && el <= hi) ranges then 1 else 0)
       0 ingredients)

let part2 (ranges, _) =
  let fst_range, ranges =
    match List.sort (fun (a, _) (b, _) -> a - b) ranges with
    | fst :: rem -> (fst, rem)
    | _ -> raise (Invalid_argument "There's gotta be more than one range")
  in
  let rec merge_ranges (hd_a, hd_b) = function
    | (nxt_a, nxt_b) :: rem ->
        if nxt_a <= hd_b || hd_a == nxt_a then merge_ranges (hd_a, (max hd_b nxt_b)) rem
        else (hd_a, hd_b) :: merge_ranges (nxt_a, nxt_b) rem
    | [] -> [ (hd_a, hd_b) ]
  in
  let merged = merge_ranges fst_range ranges in
  Some (List.fold_left (fun acc (lo, hi) -> hi - lo + 1 + acc) 0 merged)

let test_data =
  [ "3-5"; "10-14"; "16-20"; "12-18"; ""; "1"; "5"; "8"; "11"; "17"; "32" ]

let%test _ = part1 @@ parse test_data = Some 3
let%test _ = part2 @@ parse test_data = Some 14

let run input_location =
  let input = read_lines input_location |> parse in
  printf "Day 5\n";
  (match part1 input with
  | Some ans -> printf "Part 1: %d\n" ans
  | None -> print_endline "Part 1 is None");
  match part2 input with
  | Some ans -> printf "Part 2: %d\n" ans
  | None -> print_endline "Part 2 is None"
