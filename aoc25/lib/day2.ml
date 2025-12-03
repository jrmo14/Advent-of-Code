open Printf
open Util

let parse input =
  List.hd input |> String.split_on_char ','
  |> List.map (fun s ->
      match String.split_on_char '-' s with
      | [ l; h ] -> (int_of_string l, int_of_string h)
      | _ -> raise (Invalid_argument "uhhhh"))

let count_digits num =
  let rec inner num acc =
    if num == 0 then acc else inner (num / 10) (1 + acc)
  in
  inner num 0

let pow base exp =
  let rec inner exp acc =
    if exp <= 0 then acc else inner (exp - 1) (acc * base)
  in
  inner exp 1

let part1 input =
  Some
    (List.fold_left
       (fun acc (lo, hi) ->
         (Seq.ints lo
         |> Seq.take (hi - lo + 1)
         |> Seq.fold_left
              (fun acc num ->
                let num_digits = count_digits num in
                let div = pow 10 (num_digits / 2) in
                let lhs = num / div in
                let rhs = num - (lhs * div) in
                if lhs == rhs then num + acc else acc)
              0)
         + acc)
       0 input)

let contains_pat num =
  let rec check_pat num pat pat_pow =
    if num == 0 then false
    else if num == pat then true
    else if num mod pat_pow == pat then check_pat (num / pat_pow) pat pat_pow
    else false
  in
  let overall_num_digits = count_digits num in
  let rec check_all_pats pat_size =
    if pat_size >= (overall_num_digits / 2) + 1 then false
    else
      let pat_pow = pow 10 pat_size in
      if check_pat num (num mod pat_pow) pat_pow then true
      else check_all_pats (pat_size + 1)
  in
  check_all_pats 1

let part2 input =
  Some
    (List.fold_left
       (fun acc (lo, hi) ->
         (lo -- (hi + 1)
         |> Seq.fold_left
              (fun acc num -> if contains_pat num then num + acc else acc)
              0)
         + acc)
       0 input)

let test_data =
  "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

let%test _ =
  parse [ test_data ]
  = [
      (11, 22);
      (95, 115);
      (998, 1012);
      (1188511880, 1188511890);
      (222220, 222224);
      (1698522, 1698528);
      (446443, 446449);
      (38593856, 38593862);
      (565653, 565659);
      (824824821, 824824827);
      (2121212118, 2121212124);
    ]

let%test _ = part1 @@ parse [ test_data ] = Some 1227775554
let%test _ = part2 @@ parse [ test_data ] = Some 4174379265

let run input_location =
  let input = read_lines input_location |> parse in
  printf "Day 2\n";
  (match part1 input with
  | Some ans -> printf "Part 1: %d\n" ans
  | None -> print_endline "Part 1 is None");
  match part2 input with
  | Some ans -> printf "Part 2: %d\n" ans
  | None -> print_endline "Part 2 is None"
