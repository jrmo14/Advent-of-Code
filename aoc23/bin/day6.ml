open Printf
open Util
exception ParseError of string

let parse = function
  | [ time; distance ] ->
      let parse_space_list line =
        String.split_on_char ' ' line
        |> List.tl
        |> List.filter_map (fun v ->
               if String.length v != 0 then Some (int_of_string v) else None)
      in
      let time_list = parse_space_list time in
      let distance_list = parse_space_list distance in
      (time_list, distance_list)
  | _ -> raise (ParseError "There are not 2 lines")

let input = parse (read_lines "res/day6")

let quadratic_special b c =
  let s = Float.sqrt ((b *. b) -. (4. *. c)) in
  (-.((-.b +. s) /. 2.), -.((-.b -. s) /. 2.))

let part1 input =
  let time, distance = input in
  Some
    (zip time distance
    |> List.map (fun (a, b) ->
           let lo, hi =
             match quadratic_special (float_of_int a) (float_of_int b) with
             | lo, hi when lo > hi -> (hi, lo)
             | x -> x
           in
           int_of_float
             (if Float.equal (Float.ceil hi) hi then hi -. 1. else Float.ceil hi)
           - int_of_float (Float.ceil lo))
    |> List.fold_left ( * ) 1)

let rec count_tens v = if v >= 10 then 1 + count_tens (v / 10) else 0
let rec exp n p = if p > 0 then n * exp n (p - 1) else 1

let part2 input =
  let time, distance = input in
  let total_time, total_dist =
    zip time distance
    |> List.fold_left
         (fun (time_acc, dist_acc) (t, d) ->
           ( (time_acc * exp 10 (1 + count_tens t)) + t,
             (dist_acc * exp 10 (1 + count_tens d)) + d ))
         (0, 0)
  in
  let lo, hi =
    match
      quadratic_special (float_of_int total_time) (float_of_int total_dist)
    with
    | lo, hi when lo > hi -> (hi, lo)
    | x -> x
  in
  Some
    (int_of_float
       (if Float.equal (Float.ceil hi) hi then hi -. 1. else Float.ceil hi)
    - int_of_float (Float.ceil lo))

let run =
  printf "Day 6\n";
  (match part1 input with
  | Some ans -> printf "Part 1: %d\n" ans
  | None -> print_endline "Part 1 is None");
  match part2 input with
  | Some ans -> printf "Part 2: %d\n\n" ans
  | None -> print_endline "Part 2 is None"
