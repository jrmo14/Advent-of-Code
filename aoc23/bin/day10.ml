open Printf
open Util

type tile = Start | Ground | SE | SW | NW | NE | EW | NS

let parse input =
  List.map
    (fun l ->
      String.to_seq l
      |> Seq.map (function
           | '|' -> NS
           | '-' -> EW
           | 'L' -> NE
           | 'J' -> NW
           | '7' -> SW
           | 'F' -> SE
           | '.' -> Ground
           | 'S' -> Start
           | t -> raise (Invalid_argument (Printf.sprintf "Unknown tile type '%c'" t)))
      |> Array.of_seq)
    input
  |> Array.of_list

let traverse_pipe prev_x prev_y x y = function
  | NS -> if prev_y < y then (x, (y + 1) )else (x, (y - 1))
  | EW -> if prev_x < x then ((x + 1), y) else ((x - 1), y)
  | _ -> raise Not_found

let part1 input = let _start_loc = 
  Array.find_mapi (fun row_id row -> Array.find_mapi (fun col_id el -> match el with | Start -> Some (row_id, col_id)
  | _ -> None) row) input in None

let part2 _input = None

let run =
  let input = read_lines "res/day10" |> parse in
  printf "Day 10\n";
  (match part1 input with
  | Some ans -> printf "Part 1: %d\n" ans
  | None -> print_endline "Part 1 is None");
  match part2 input with
  | Some ans -> printf "Part 2: %d\n\n" ans
  | None -> print_endline "Part 2 is None\n"
