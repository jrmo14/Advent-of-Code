open Printf
open Util

let parse input =
  List.map (fun line -> String.to_seq line |> Array.of_seq) input
  |> Array.of_list

let valid_moves loc =
  [
    point_add loc (-1, 0);
    point_add loc (1, 0);
    point_add loc (0, -1);
    point_add loc (0, 1);
  ]

let build_region map root seen =
  let bound_row = Array.length map in
  let bound_col = Array.length map.(0) in
  let bounds = (bound_col, bound_row) in
  let map_lookup (c, r) = map.(r).(c) in
  let symbol = map_lookup root in
  let region_contents = Hashtbl.create 0 in
  let rec flood_region cur =
    Hashtbl.replace region_contents cur 0;
    Hashtbl.replace seen cur 0;
    valid_moves cur
    |> List.fold_left
         (fun acc pt ->
           if in_bounds bounds pt && map_lookup pt == symbol then
             if not (Hashtbl.mem region_contents pt) then acc + flood_region pt
             else acc
           else acc + 1)
         0
  in
  let perim = flood_region root in
  let area = Hashtbl.length region_contents in
  (* printf "Region (%c) %s: area: %d perimeter: %d score = %d\n" symbol (string_of_point root) *)
  (* area perim (area * perim); *)
  (area, perim, region_contents)

let part1 input =
  let seen = Hashtbl.create 0 in
  Some
    (List.fold_left
       (fun acc row ->
         List.fold_left
           (fun acc col ->
             if not (Hashtbl.mem seen (col, row)) then
               let area, perim, _ = build_region input (col, row) seen in
               acc + (area * perim)
             else acc)
           acc
           (0 -- (Array.length input.(row) - 1)))
       0
       (0 -- (Array.length input - 1)))

let part2 input =
  let seen = Hashtbl.create 0 in
  (* let bound_row = Array.length input in *)
  (* let bound_col = Array.length input.(0) in *)
  (* let bounds = (bound_col, bound_row) in *)

  let corner_offsets =
    all_pairs array_dirs
    |> List.filter_map (fun (a, b) ->
           let x = point_add a b in
           if point_eq (0, 0) x then None else Some x)
  in
  (* let map_lookup (c, r) = input.(r).(c) in *)

  let count_corners region =
    let corners = Hashtbl.create 0 in 
    let _= Hashtbl.to_seq_keys region
    |> Seq.fold_left
         (fun acc pt ->
           corner_offsets
           |> List.fold_left
                (fun acc c ->
                  (* let sym = map_lookup pt in *)
                  let decompose (c, r) =
                    (point_add pt (c, 0), point_add pt (0, r))
                  in
                  let diag = point_add c pt in
                  let a, b = decompose c in
                  if
                    (not (Hashtbl.mem region diag))
                    && not (Hashtbl.mem region a ^^ Hashtbl.mem region b)
                  then acc + 1
                  else acc)
                acc)
         0 in
      Hashtbl.to_seq_values corners |> Seq.fold_left (fun acc v -> acc + v) 0
  in

  Some
    (List.fold_left
       (fun acc row ->
         List.fold_left
           (fun acc col ->
             if not (Hashtbl.mem seen (col, row)) then
               let area, _, region = build_region input (col, row) seen in
               let n_corners = count_corners region in
               acc + (area * n_corners)
             else acc)
           acc
           (0 -- (Array.length input.(row) - 1)))
       0
       (0 -- (Array.length input - 1)))

let run () =
  let input = read_lines "res/day12" |> parse in
  printf "Day 12\n";
  (match part1 input with
  | Some ans -> printf "Part 1: %d\n" ans
  | None -> print_endline "Part 1 is None");
  match part2 input with
  | Some ans -> printf "Part 2: %d\n" ans
  | None -> print_endline "Part 2 is None"
