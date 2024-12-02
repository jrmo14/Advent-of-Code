open Printf

let parse input =
  List.map
    (fun _line -> "")
    input

let part1 _input =None
let part2 _input = None
let run =
  let input = Util.read_lines "res/day3" |> parse in
  printf "Day 3\n";
  (match part1 input with
  | Some ans -> printf "Part 1: %d\n" ans
  | None -> print_endline "Part 1 is None");
  match part2 input with
  | Some ans -> printf "Part 1: %d\n" ans
  | None -> print_endline "Part 1 is None"
