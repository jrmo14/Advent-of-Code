open Printf
open Util

type rotation = Left of int | Right of int

let parse input =
  List.map
    (fun line ->
      let amnt = int_of_string (String.sub line 1 (String.length line - 1)) in
      match String.sub line 0 1 with
      | "L" -> Left amnt
      | "R" -> Right amnt
      | _ -> raise (Invalid_argument "Direction doesn't make sense"))
    input

let part1 input =
  let _, count =
    List.fold_left
      (fun (pos, cnt) el ->
        let new_pos =
          match el with
          | Left v ->
              let v = v mod 100 in
              if v > pos then 100 - (v - pos) else pos - v
          | Right v -> (pos + v) mod 100
        in
        if new_pos == 0 then (new_pos, cnt + 1) else (new_pos, cnt))
      (50, 0) input
  in
  Some count

let part2 input =
  let _, count =
    List.fold_left
      (fun (pos, cnt) el ->
        let new_pos, crossings =
          match el with
          | Left v ->
              let full_rotations = v / 100 in
              let v = v mod 100 in
              if v > pos then (100 - (v - pos), 1 + full_rotations)
              else (pos - v, full_rotations)
          | Right v ->
              let full_rotations = v / 100 in
              let v = v mod 100 in
              if pos + v >= 100 then (pos + v - 100, 1 + full_rotations)
              else (pos + v, full_rotations)
        in
        (new_pos, cnt + crossings))
      (50, 0) input
  in
  Some count

let test_data =
  [ "L68"; "L30"; "R48"; "L5"; "R60"; "L55"; "L1"; "L99"; "R14"; "L82" ]

let%test _ = part1 @@ parse test_data = Some 3
let%test _ = part2 @@ parse test_data = Some 6

let run input_location =
  let input = read_lines input_location |> parse in
  printf "Day 1\n";
  (match part1 input with
  | Some ans -> printf "Part 1: %d\n" ans
  | None -> print_endline "Part 1 is None");
  match part2 input with
  | Some ans -> printf "Part 2: %d\n" ans
  | None -> print_endline "Part 2 is None"
