open Printf
open Util

let parse input =
  let lst, _ =
    List.fold_left
      (fun (acc, row) line ->
        let acc, _ =
          List.fold_left
            (fun (acc, col) e ->
              match e with
              | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' ->
                  ((col, row, e) :: acc, col + 1)
              | _ -> (acc, col + 1))
            (acc, 0)
            (String.to_seq line |> List.of_seq)
        in
        (acc, row + 1))
      ([], 0) input
  in
  let antenna_locs = Hashtbl.create 0 in
  List.iter
    (fun (c, r, e) ->
      let old =
        match Hashtbl.find_opt antenna_locs e with Some v -> v | None -> []
      in
      Hashtbl.replace antenna_locs e ((c, r) :: old))
    lst;
  (antenna_locs, (String.length (List.hd input), List.length input))

let part1 (antenna_locs, bounds) =
  let count_antinodes bounds ant =
    all_pairs ant
    |> List.fold_left
         (fun acc (a, b) ->
           let valid = in_bounds bounds in
           let delta1 = point_sub a b in
           let delta2 = point_sub b a in
           let x = point_add a delta1 in
           let y = point_add b delta2 in
           let acc = if valid x then x :: acc else acc in
           if valid y then y :: acc else acc)
         []
  in
  let node_locs = Hashtbl.create 0 in
  Hashtbl.to_seq_values antenna_locs
  |> Seq.iter (fun ant ->
         List.iter
           (fun x ->
             let _ = Hashtbl.replace node_locs x 0 in
             ())
           (count_antinodes bounds ant));
  Some (Hashtbl.length node_locs)

let part2 (antenna_locs, bounds) =
  let count_antinodes bounds ant =
    let step_nodes acc (a, b) =
      let rec inner e delta acc =
        if in_bounds bounds e then inner (point_add e delta) delta (e :: acc)
        else acc
      in
      inner b (point_sub b a) (inner a (point_sub a b) acc)
    in
    all_pairs ant |> List.fold_left step_nodes []
  in
  let node_locs = Hashtbl.create 0 in
  Hashtbl.to_seq_values antenna_locs
  |> Seq.iter (fun antennas ->
         List.iter
           (fun x ->
             let _ = Hashtbl.replace node_locs x 0 in
             ())
           (count_antinodes bounds antennas));
  Some (Hashtbl.length node_locs)

let run () =
  let input = read_lines "res/day8" |> parse in
  printf "Day 8\n";
  (match part1 input with
  | Some ans -> printf "Part 1: %d\n" ans
  | None -> print_endline "Part 1 is None");
  match part2 input with
  | Some ans -> printf "Part 2: %d\n" ans
  | None -> print_endline "Part 2 is None"
