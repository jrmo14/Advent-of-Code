open Printf
open Util

let parse input =
  List.map (fun line -> String.to_seq line |> Array.of_seq) input
  |> Array.of_list

let check_adj arr (x, y) =
  let offsets =
    Seq.product (Seq.ints (-1) |> Seq.take 3) (Seq.ints (-1) |> Seq.take 3)
    |> Seq.filter (fun (x, y) -> x != 0 || y != 0)
  in
  let max_x = Array.length arr in
  let max_y = Array.length arr.(0) in
  4
  > Seq.fold_left
      (fun acc (off_x, off_y) ->
        let x = off_x + x in
        let y = off_y + y in
        acc
        +
        if x < 0 || y < 0 || x >= max_x || y >= max_y then 0
        else if arr.(x).(y) == '.' then 0
        else 1)
      0 offsets

let part1 input =
  let max_x = Array.length input in
  let max_y = Array.length input.(0) in
  let coords =
    Seq.product (Seq.ints 0 |> Seq.take max_x) (Seq.ints 0 |> Seq.take max_y)
  in
  let c =
    Seq.fold_left
      (fun acc (x, y) ->
        acc + if input.(x).(y) == '@' && check_adj input (x, y) then 1 else 0)
      0 coords
  in
  Some c

let part2 input =
  let max_x = Array.length input in
  let max_y = Array.length input.(0) in
  (* TODO: this might be slow because of 2d array acesses... maybe try flattening *)
  (* let flat_array = Array.concat (Array.to_list input) in *)
  let mark () =
    let coords =
      Seq.product (Seq.ints 0 |> Seq.take max_x) (Seq.ints 0 |> Seq.take max_y)
    in
    Seq.fold_left
      (fun acc (x, y) ->
        if input.(x).(y) != '.' && check_adj input (x, y) then (x, y) :: acc
        else acc)
      [] coords
  in
  let rec inner acc =
    let to_remove = mark () in
    let n_to_remove = List.length to_remove in
    if 0 == n_to_remove then acc
    else (
      List.iter (fun (x, y) -> input.(x).(y) <- '.') to_remove;
      (inner [@tailcall]) (acc + n_to_remove))
  in
  Some (inner 0)

let test_data =
  [
    "..@@.@@@@.";
    "@@@.@.@.@@";
    "@@@@@.@.@@";
    "@.@@@@..@.";
    "@@.@@@@.@@";
    ".@@@@@@@.@";
    ".@.@.@.@@@";
    "@.@@@.@@@@";
    ".@@@@@@@@.";
    "@.@.@@@.@.";
  ]

let%test _ = part1 @@ parse test_data = Some 13
let%test _ = part2 @@ parse test_data = Some 43

let run input_location =
  let input = read_lines input_location |> parse in
  printf "Day 4\n";
  (match part1 input with
  | Some ans -> printf "Part 1: %d\n" ans
  | None -> print_endline "Part 1 is None");
  match part2 input with
  | Some ans -> printf "Part 2: %d\n" ans
  | None -> print_endline "Part 2 is None"
