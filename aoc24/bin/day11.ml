open Printf
open Util

let parse input =
  List.hd input |> String.split_on_char ' ' |> List.map int_of_string

let count_digits n =
  let rec inner n idx = if n == 0 then idx else inner (n / 10) (idx + 1) in
  if n == 0 then 1 else inner n 0

let split_number n =
  let rec inner_trim e cnt =
    if cnt == 0 then e else inner_trim (e / 10) (cnt - 1)
  in
  let n_digits = count_digits n in
  let top = inner_trim n (n_digits / 2) in
  let bottom = n - (top * pow 10 (n_digits / 2)) in
  (top, bottom)

(**
   - If the stone is engraved with the number 0, 
       it is replaced by a stone engraved with the number 1.
   - If the stone is engraved with a number that has an even number of digits, 
       it is replaced by two stones.
       The left half of the digits are engraved on the new left stone, 
       and the right half of the digits are engraved on the new right stone.
       (The new numbers don't keep extra leading zeroes: 
           1000 would become stones 10 and 0.)
   - If none of the other rules apply, the stone is replaced by a new stone;
       the old stone's number multiplied by 2024 is engraved on the new stone.
*)
let blink = function
  | 0 -> [ 1 ]
  | x when count_digits x mod 2 == 0 ->
      let top, bottom = split_number x in
      [ top; bottom ]
  | x -> [ x * 2024 ]

let part1 input =
  let apply_blink state =
    List.fold_left (fun acc el -> blink el @ acc) [] state |> List.rev
  in
  Some
    (List.length
       (List.fold_left (fun state _ -> apply_blink state) input (0 -- 24)))

let part2 input =
  let memo = Hashtbl.create 0 in
  let rec count_memoized stone limit cur =
    if limit == cur then 1
    else
      match Hashtbl.find_opt memo (stone, cur) with
      | Some count -> count
      | _ ->
          let n_prod =
            blink stone
            |> List.fold_left
                 (fun cnt s -> cnt + count_memoized s limit (cur + 1))
                 0
          in
          Hashtbl.replace memo (stone, cur) n_prod;
          n_prod
  in
  Some (List.fold_left (fun acc s -> acc + count_memoized s 75 0) 0 input)

let run =
  let input = read_lines "res/day11" |> parse in
  printf "Day 11\n";
  (match part1 input with
  | Some ans -> printf "Part 1: %d\n" ans
  | None -> print_endline "Part 1 is None");
  match part2 input with
  | Some ans -> printf "Part 2: %d\n" ans
  | None -> print_endline "Part 2 is None"
