open Printf
open Util

let parse input =
  let updates, rules =
    List.fold_left
      (fun (left, right) elem ->
        match (left, right) with
        | left, Some right -> (left, Some (elem :: right))
        | left, None when 0 == String.length elem -> (left, Some [])
        | left, right -> (elem :: left, right))
      ([], None) (List.rev input)
  in
  let rules =
    Option.get rules
    |> List.map (fun r ->
           match String.split_on_char '|' r with
           | [ x; y ] -> (int_of_string x, int_of_string y)
           | _ -> raise (Invalid_argument "Unable to parse rule "))
  in
  let updates =
    List.map
      (fun x -> String.split_on_char ',' x |> List.map int_of_string)
      updates
  in
  (rules, updates)

let get_middle lst = List.hd (skip (List.length lst / 2) lst)

let rec validate_update rules = function
  | x :: rem ->
      if all (fun y -> Hashtbl.mem rules (x, y)) rem then
        validate_update rules rem
      else false
  | _ -> true

let part1 input =
  let rules_lst, updates = input in
  let rules = Hashtbl.create (List.length rules_lst) in
  List.iter (fun rule -> Hashtbl.add rules rule 0) rules_lst;
  Some
    (List.filter (validate_update rules) updates
    |> List.map get_middle |> List.fold_left ( + ) 0)

let part2 input =
  let rules_lst, updates = input in
  let rules = Hashtbl.create (List.length rules_lst) in
  List.iter (fun rule -> Hashtbl.add rules rule 0) rules_lst;

  let fix_order lst =
    List.sort
      (fun x y ->
        if Hashtbl.mem rules (x, y) then -1
        else if Hashtbl.mem rules (y, x) then 1
        else 0)
      lst
  in
  Some
    (List.filter (fun x -> not (validate_update rules x)) updates
    |> List.map fix_order |> List.map get_middle |> List.fold_left ( + ) 0)

let run =
  let input = read_lines "res/day5" |> parse in
  printf "Day 5\n";
  (match part1 input with
  | Some ans -> printf "Part 1: %d\n" ans
  | None -> print_endline "Part 1 is None");
  match part2 input with
  | Some ans -> printf "Part 2: %d\n" ans
  | None -> print_endline "Part 2 is None"
