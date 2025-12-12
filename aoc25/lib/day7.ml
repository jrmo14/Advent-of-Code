open Printf
open Util

let parse input = input

let part1 input =
  let initial_state =
    List.hd input |> String.to_seq
    |> Seq.map (fun c -> c == 'S')
    |> Array.of_seq
  in
  let _, count =
    List.fold_left
      (fun (state, count) line ->
        let count, _ =
          String.fold_left
            (fun (count, i) c ->
              if c == '^' && state.(i) then (
                state.(i) <- false;
                state.(i - 1) <- true;
                state.(i + 1) <- true;
                (count + 1, i + 1))
              else (count, i + 1))
            (count, 0) line
        in
        (state, count))
      (initial_state, 0) (List.tl input)
  in
  Some count

let part2 input =
  let input =
    Array.of_list @@ List.map (fun l -> String.to_seq l |> Array.of_seq) input
  in
  let memoize = Hashtbl.create 0 in
  let rec simulate_timeline y x =
    if y < Array.length input then
      match Hashtbl.find_opt memoize (y, x) with
      | Some ans -> ans
      | None ->
          if input.(y).(x) == '^' then
            let count = simulate_timeline y (x + 1) + simulate_timeline y (x - 1) in
            (Hashtbl.add memoize (y,x) count;
            count)
          else simulate_timeline (y + 1) x
    else 1
  in
  let initial_x =
    Option.get @@ Array.find_index (fun e -> e == 'S') input.(0)
  in
  Some (simulate_timeline 0 initial_x)

let test_data =
  [
    ".......S.......";
    "...............";
    ".......^.......";
    "...............";
    "......^.^......";
    "...............";
    ".....^.^.^.....";
    "...............";
    "....^.^...^....";
    "...............";
    "...^.^...^.^...";
    "...............";
    "..^...^.....^..";
    "...............";
    ".^.^.^.^.^...^.";
    "...............";
  ]

let%test _ = part1 @@ parse test_data = Some 21
let%test _ = part2 @@ parse test_data = Some 40

let run input_location =
  let input = read_lines input_location |> parse in
  printf "Day 7\n";
  (match part1 input with
  | Some ans -> printf "Part 1: %d\n" ans
  | None -> print_endline "Part 1 is None");
  match part2 input with
  | Some ans -> printf "Part 2: %d\n" ans
  | None -> print_endline "Part 2 is None"
