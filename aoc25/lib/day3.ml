open Printf
open Util

let parse input =
  List.map
    (fun line ->
      String.to_seq line
      |> Seq.map (fun c -> int_of_char c - int_of_char '0')
      |> Array.of_seq)
    input
  |> Array.of_list

let compute_joltage_simple bank =
  let rec inner tens ones i =
    if i + 1 < Array.length bank then
      let cur = bank.(i) in
      let nxt = bank.(i + 1) in
      if cur > tens then inner cur nxt (i + 1)
      else if nxt > ones then inner tens nxt (i + 1)
      else inner tens ones (i + 1)
    else (tens * 10) + ones
  in
  inner 0 0 0

let compute_joltage n bank =
  (* printf "["; *)
  (* Array.iter (fun e -> printf " %d " e) bank; *)
  (* printf "]\n"; *)
  let arr = Array.make n 0 in
  let rec inner_loop read_idx write_idx rem_to_read find_idx =
    (* printf "Inner loop read (%d) write (%d) rem (%d) find(%d)\n" read_idx write_idx rem_to_read find_idx; *)
    if read_idx + rem_to_read - 1< Array.length bank then
      if arr.(write_idx) < bank.(read_idx) then (
        (* printf "Set (%d) = %d read %d\n" write_idx bank.(read_idx) read_idx; *)
        arr.(write_idx) <- bank.(read_idx);
        inner_loop (read_idx + 1) write_idx rem_to_read read_idx)
      else inner_loop (read_idx + 1) write_idx rem_to_read find_idx
    else find_idx
  in
  let rec lead_scan lead_write_idx lead_read_idx =
    (* printf "Lead scan write %d read %d\n" lead_write_idx lead_read_idx; *)
    if lead_write_idx < n then
      let rem_to_read = n - lead_write_idx in
      let find_idx = inner_loop lead_read_idx lead_write_idx rem_to_read 0 in
      (* printf "Retunr find_idx %d\n" find_idx; *)
      lead_scan (lead_write_idx + 1) (find_idx+1)
  in
  lead_scan 0 0;
  let x = Array.fold_left (fun acc i -> (acc * 10) + i) 0 arr in
  (* printf "RETURNING %d\n" x; *)
  x
  

let compute_all_joltage compute input =
  let joltage = Array.fold_left (fun acc bank -> acc + compute bank) 0 input in
  (* printf "Joltage is %d\n" joltage; *)
  Some (joltage)

let part1 input = compute_all_joltage compute_joltage_simple input
let part2 input = compute_all_joltage (compute_joltage 12) input

let test_data =
  [ "987654321111111";  "234234234234278"; "818181911112111"; "811111111111119" ]

let%test _ = parse [ "1234" ] = [| [| 1; 2; 3; 4 |] |]
let%test _ = part1 @@ parse test_data = Some 357
let%test _ = part2 @@ parse test_data = Some 3121910778619

let run input_location =
  let input = read_lines input_location |> parse in
  printf "Day 3\n";
  (match part1 input with
  | Some ans -> printf "Part 1: %d\n" ans
  | None -> print_endline "Part 1 is None");
  match part2 input with
  | Some ans -> printf "Part 2: %d\n" ans
  | None -> print_endline "Part 2 is None"
