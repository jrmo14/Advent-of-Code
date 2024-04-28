open Printf
open Util
open Seq

let parse input =
  List.map
    (fun line -> String.split_on_char ' ' line |> List.map int_of_string)
    input

let compute_derivatives readings =
  pairs readings |> List.map (fun (a, b) -> b - a)

let rec produce_derivatives readings seq () =
  if all (fun e -> e == 0) readings || List.length readings <= 1 then Nil
  else
    let deriv = compute_derivatives readings in
    Cons (deriv, produce_derivatives deriv seq)

let predict_fwd lst =
  (fold_left_until
     (fun acc x -> Some (last_itm x :: acc))
     []
     (produce_derivatives lst ())
  |> List.fold_left ( + ) 0)
  + last_itm lst

let part1 input = Some (List.map predict_fwd input |> List.fold_left ( + ) 0)

let predict_back lst =
  List.hd lst
  - (fold_left_until
       (fun acc x -> Some (List.hd x :: acc))
       []
       (produce_derivatives lst ())
    |> List.fold_left (fun a b -> b - a) 0)

let part2 input = Some (List.map predict_back input |> List.fold_left ( + ) 0)

let run =
  let input = read_lines "res/day9" |> parse in
  printf "Day 9\n";
  (match part1 input with
  | Some ans -> printf "Part 1: %d\n" ans
  | None -> print_endline "Part 1 is None");
  match part2 input with
  | Some ans -> printf "Part 2: %d\n\n" ans
  | None -> print_endline "Part 2 is None"
