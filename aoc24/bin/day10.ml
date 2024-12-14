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

let valid_moves loc bounds =
  [
    (let left = point_add (-1, 0) loc in
     if in_bounds bounds left then Some left else None);
    (let right = point_add (1, 0) loc in
     if in_bounds bounds right then Some right else None);
    (let up = point_add (0, -1) loc in
     if in_bounds bounds up then Some up else None);
    (let down = point_add (0, 1) loc in
     if in_bounds bounds down then Some down else None);
  ]
  |> List.filter_map (fun x -> x)

let part1 input =
  let bounds = (Array.length input.(0), Array.length input) in

  let trailheads =
    List.fold_left
      (fun acc row ->
        List.fold_left
          (fun acc col ->
            if input.(row).(col) == 0 then (col, row) :: acc else acc)
          acc
          (0 -- (Array.length input.(row) - 1)))
      []
      (0 -- (Array.length input - 1))
  in

  let count_peaks trailhead =
    let visited = Hashtbl.create 0 in
    let rec dfs pos =
      Hashtbl.replace visited pos 0;
      let col, row = pos in
      if input.(row).(col) == 9 then 1
      else
        valid_moves pos bounds
        |> List.filter (fun (nxt_col, nxt_row) ->
               let cur_height = input.(row).(col) in
               let nxt_height = input.(nxt_row).(nxt_col) in
               cur_height + 1 == nxt_height
               && not (Hashtbl.mem visited (nxt_col, nxt_row)))
        |> List.fold_left (fun acc p -> acc + dfs p) 0
    in
    dfs trailhead
  in

  Some
    (List.fold_left
       (fun acc trailhead -> acc + count_peaks trailhead)
       0 trailheads)

let part2 input =
  let bounds = (Array.length input.(0), Array.length input) in

  let search height =
    List.fold_left
      (fun acc row ->
        List.fold_left
          (fun acc col ->
            if input.(row).(col) == height then (col, row) :: acc else acc)
          acc
          (0 -- (Array.length input.(row) - 1)))
      []
      (0 -- (Array.length input - 1))
  in

  let trailheads = search 0 in
  let peaks = search 9 in

  let count_trails trailhead =
    let visited = Hashtbl.create 0 in
    let rec dfs pos =
      (match Hashtbl.find_opt visited pos with
      | Some count -> Hashtbl.replace visited pos (count + 1)
      | None -> Hashtbl.add visited pos 1);
      let col, row = pos in
      if input.(row).(col) == 9 then 1
      else
        valid_moves pos bounds
        |> List.filter (fun (nxt_col, nxt_row) ->
               let cur_height = input.(row).(col) in
               let nxt_height = input.(nxt_row).(nxt_col) in
               cur_height + 1 == nxt_height)
        |> List.fold_left (fun acc p -> acc + dfs p) 0
    in
    let _ = dfs trailhead in
    List.fold_left
      (fun acc p ->
        match Hashtbl.find_opt visited p with Some c -> acc + c | None -> acc)
      0 peaks
  in

  Some
    (List.fold_left
       (fun acc trailhead -> acc + count_trails trailhead)
       0 trailheads)

let run =
  let input = read_lines "res/day10" |> parse in
  printf "Day 10\n";
  (match part1 input with
  | Some ans -> printf "Part 1: %d\n" ans
  | None -> print_endline "Part 1 is None");
  match part2 input with
  | Some ans -> printf "Part 2: %d\n" ans
  | None -> print_endline "Part 2 is None"
