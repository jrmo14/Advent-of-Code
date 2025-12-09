open Printf
open Util

let number_line line =
  String.split_on_char ' ' line
  |> List.filter_map (fun chunk ->
      if String.length chunk > 0 then Some (int_of_string chunk) else None)

let rec zip a b =
  match (a, b) with i :: a, j :: b -> (i, j) :: zip a b | _ -> []

let parse_ops ops_line =
  String.split_on_char ' ' ops_line
  |> List.filter_map (fun chunk ->
      if String.length chunk > 0 then Some (chunk.[0] == '+') else None)

let parse input =
  List.fold_left
    (fun (cols, _) line ->
      match line.[0] with
      | '*' | '+' -> (cols, parse_ops line)
      | _ ->
          let numbers = number_line line in
          let acc =
            if 0 == List.length cols then
              List.init (List.length numbers) (fun _ -> [])
            else cols
          in
          (List.map (fun (el, col) -> el :: col) (zip numbers acc), []))
    ([], []) input

let part1 (cols, ops) =
  Some
    (List.fold_left
       (fun acc (col, do_add) ->
         acc
         +
         if do_add then List.fold_left (fun acc num -> acc + num) 0 col
         else List.fold_left (fun acc num -> acc * num) 1 col)
       0 (zip cols ops))

let parse2 input =
  let numbers, operations =
    List.fold_left
      (fun (rows, _) line ->
        match line.[0] with
        | '*' | '+' -> (rows, parse_ops line)
        | _ -> ((String.to_seq line |> Array.of_seq) :: rows, []))
      ([], []) input
  in
  (Array.of_list @@ List.rev numbers, operations)

let part2 (num_mat, ops) =
  let ceph_num x =
    let num =
      0 -- Array.length num_mat
      |> Seq.fold_left
           (fun acc n ->
             let c = num_mat.(n).(x) in
             if c == ' ' then acc
             else (acc * 10) + (int_of_char c - int_of_char '0'))
           0
    in
    num
  in

  let all_empty x =
    0 -- Array.length num_mat
    |> Seq.fold_left (fun acc n -> acc && num_mat.(n).(x) == ' ') true
  in

  let cur_eq, eqs =
    0 -- Array.length num_mat.(0)
    |> Seq.fold_left
         (fun (cur_eq, eqs) x ->
           if all_empty x then ([], cur_eq :: eqs)
           else (ceph_num x :: cur_eq, eqs))
         ([], [])
  in
  let eq_nums = List.rev (cur_eq :: eqs) in
  (* List.iter *)
    (* (fun eq -> *)
      (* List.iter (printf "%d,") eq; *)
      (* printf "\n") *)
    (* eq_nums; *)
  Some
    (List.fold_left
       (fun acc (col, do_add) ->
         acc
         +
         if do_add then List.fold_left (fun acc num -> acc + num) 0 col
         else List.fold_left (fun acc num -> acc * num) 1 col)
       0 (zip eq_nums ops))

let test_data =
  [ "123 328  51 64 "; " 45 64  387 23 "; "  6 98  215 314"; "*   +   *   +  " ]

let%test _ = part1 @@ parse test_data = Some 4277556
let%test _ = part2 @@ parse2 test_data = Some 3263827

let run input_location =
  let input = read_lines input_location in
  printf "Day 6\n";
  (match part1 @@ parse input with
  | Some ans -> printf "Part 1: %d\n" ans
  | None -> print_endline "Part 1 is None");
  match part2 @@ parse2 input with
  | Some ans -> printf "Part 2: %d\n" ans
  | None -> print_endline "Part 2 is None"
