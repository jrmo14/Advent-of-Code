open Printf
open Util

let parse input =
  List.map
    (fun line ->
      match String.split_on_char ':' line with
      | [ target; vals ] ->
          ( int_of_string target,
            String.split_on_char ' ' vals |> List.tl |> List.map int_of_string
          )
      | _ -> raise (Invalid_argument "Unable to parse"))
    input

(* let concat_digits a b = int_of_string (string_of_int a ^ string_of_int b) *)

(* Testing showed that this is 15-20x faster *)
let concat_digits a b =
  let rec inner aP bP = if bP == 0 then aP + b else inner (aP * 10) (bP / 10) in
  inner a b

let check ~concat_enable target vals =
  let rec inner acc = function
    | x :: xs ->
        if acc <= target then
          inner (acc + x) xs
          || inner (acc * x) xs
          || (concat_enable && inner (concat_digits acc x) xs)
        else false
    | [] -> acc == target
  in
  inner 0 vals

let part1 input =
  Some
    (List.fold_left
       (fun acc (target, vals) ->
         if check ~concat_enable:false target vals then acc + target else acc)
       0 input)

let part2 input =
  Some
    (List.fold_left
       (fun acc (target, vals) ->
         if check ~concat_enable:true target vals then acc + target else acc)
       0 input)

let run =
  let input = read_lines "res/day7" |> parse in
  printf "Day 7\n";
  (match part1 input with
  | Some ans -> printf "Part 1: %d\n" ans
  | None -> print_endline "Part 1 is None");
  match part2 input with
  | Some ans -> printf "Part 2: %d\n" ans
  | None -> print_endline "Part 2 is None"
