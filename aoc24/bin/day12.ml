open Printf
open Util


let parse input = List.map (fun line -> String.to_seq line |> Array.of_seq) input |> Array.of_list


let part1 _input = None
let part2 _input = None




let run () =
  let input = read_lines "res/day12" |> parse in
  printf "Day 12\n";
  (match part1 input with
  | Some ans -> printf "Part 1: %d\n" ans
  | None -> print_endline "Part 1 is None");
  match part2 input with
  | Some ans -> printf "Part 2: %d\n" ans
  | None -> print_endline "Part 2 is None"
