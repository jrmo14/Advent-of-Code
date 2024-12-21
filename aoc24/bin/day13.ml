open Printf
open Util

type machine = { a : int * int; b : int * int; prize : int * int }

let parse_machine chunk =
  let extract s c =
    let s = List.tl (String.split_on_char c s) in
    let x = int_of_string (List.hd (String.split_on_char ',' (List.hd s))) in
    let y = int_of_string (last_itm s) in
    (x, y)
  in
  match take 3 chunk with
  | [ a; b; prize ] ->
      Some { a = extract a '+'; b = extract b '+'; prize = extract prize '=' }
  | _ -> None

let parse input = List.filter_map parse_machine (chunk 4 input)

let compute_cost machine =
  let x1, y1 = machine.a in
  let x2, y2 = machine.b in
  let xg, yg = machine.prize in
  let a = ((xg * y2) - (yg * x2)) / ((x1 * y2) - (y1 * x2)) in
  let b = (xg - (x1 * a)) / x2 in
  if (a * x1) + (b * x2) == xg && (a * y1) + (b * y2) == yg then
    Some ((a * 3) + b)
  else None

let part1 input =
  Some
    (List.fold_left
       (fun acc m ->
         match compute_cost m with Some c -> acc + c | None -> acc)
       0 input)

let part2 input =
  Some
    (List.fold_left
       (fun acc m ->
         match
           compute_cost
             { m with prize = point_add (10000000000000, 10000000000000) m.prize }
         with
         | Some c -> acc + c
         | None -> acc)
       0 input)

let run () =
  let input = read_lines "res/day13" |> parse in
  printf "Day 13\n";
  (match part1 input with
  | Some ans -> printf "Part 1: %d\n" ans
  | None -> print_endline "Part 1 is None");
  match part2 input with
  | Some ans -> printf "Part 2: %d\n" ans
  | None -> print_endline "Part 2 is None"
