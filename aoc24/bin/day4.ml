open Printf
open Util

let parse input = List.map (fun line -> line) input

let contain_count substr haystack =
  let substr_len = String.length substr in
  let rec inner substr haystack acc =
    if String.length substr > String.length haystack then acc
    else if String.starts_with ~prefix:substr haystack then
      let new_haystack =
        String.sub haystack substr_len (String.length haystack - substr_len)
      in
      (inner [@tailcall]) substr new_haystack (acc + 1)
    else
      (inner [@tailcall]) substr
        (String.sub haystack 1 (String.length haystack - 1))
        acc
  in
  inner substr haystack 0

let contain_indices substr haystack =
  let substr_len = String.length substr in
  let rec inner substr haystack idx acc =
    if substr_len > String.length haystack then acc
    else if String.starts_with ~prefix:substr haystack then
      let new_haystack =
        String.sub haystack substr_len (String.length haystack - substr_len)
      in
      (inner [@tailcall]) substr new_haystack (idx + substr_len) (idx :: acc)
    else
      (inner [@tailcall]) substr
        (String.sub haystack 1 (String.length haystack - 1))
        (idx + 1) acc
  in
  inner substr haystack 0 []

let part1 input =
  let search row =
    let s = List.to_seq row |> String.of_seq in
    contain_count "XMAS" s + contain_count "SAMX" s
  in

  let rdiags =
    List.mapi
      (* Add a delimiter to the side of the matrix before rotation to break
         instances where we wrap around...
      *)
        (fun i row -> rotate i ('|' :: (String.to_seq row |> List.of_seq)))
      input
    |> transpose
    |> List.fold_left (fun acc row -> acc + search row) 0
  in

  let ldiags =
    List.mapi
      (fun i row -> rotate_right i ('|' :: (String.to_seq row |> List.of_seq)))
      input
    |> transpose
    |> List.fold_left (fun acc row -> acc + search row) 0
  in

  let vertical =
    List.map (fun row -> String.to_seq row |> List.of_seq) input
    |> transpose
    |> List.fold_left (fun acc row -> acc + search row) 0
  in
  let horizontal =
    List.map
      (fun line -> contain_count "XMAS" line + contain_count "SAMX" line)
      input
    |> List.fold_left ( + ) 0
  in
  Some (rdiags + ldiags + vertical + horizontal)

let part2 input =
  let mat =
    Array.of_list (List.map (fun row -> Array.of_seq (String.to_seq row)) input)
  in

  let a_coords =
    List.flatten
      (List.map
         (fun row ->
           List.filter_map
             (fun col ->
               if mat.(row).(col) == 'A' then Some (row, col) else None)
             (0 -- (Array.length mat.(row) - 1)))
         (0 -- (Array.length mat - 1)))
  in

  let check (row, col) mat =
    if row < 1 || col < 1 then false
    else if row + 1 >= Array.length mat || col + 1 >= Array.length mat.(row)
    then false
    else
      let score =
        (if
           (mat.(row + 1).(col + 1) == 'S' && mat.(row - 1).(col - 1) == 'M')
           || (mat.(row + 1).(col + 1) == 'M' && mat.(row - 1).(col - 1) == 'S')
         then 1
         else 0)
        +
        if
          (mat.(row - 1).(col + 1) == 'S' && mat.(row + 1).(col - 1) == 'M')
          || (mat.(row - 1).(col + 1) == 'M' && mat.(row + 1).(col - 1) == 'S')
        then 1
        else 0
      in
      score == 2
  in

  Some
    (List.fold_left
       (fun acc coord -> acc + if check coord mat then 1 else 0)
       0 a_coords)

let run () =
  let input = read_lines "res/day4" in
  printf "Day 4\n";
  (match part1 input with
  | Some ans -> printf "Part 1: %d\n" ans
  | None -> print_endline "Part 1 is None");
  match part2 input with
  | Some ans -> printf "Part 2: %d\n" ans
  | None -> print_endline "Part 2 is None"
