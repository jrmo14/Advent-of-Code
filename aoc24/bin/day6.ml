open Printf
open Util

type direction = UP | DOWN | LEFT | RIGHT
type guard_state = { dir : direction; pos : int * int }

let parse input =
  let _, obstacles, initial_coords =
    List.fold_left
      (fun (i, walls, pos) row ->
        let _, walls, pos =
          row |> String.to_seq |> List.of_seq
          |> List.fold_left
               (fun (j, walls, pos) c ->
                 ( j + 1,
                   (if c == '#' then (j, i) :: walls else walls),
                   if c == '^' then Some (j, i) else pos ))
               (0, walls, pos)
        in
        (i + 1, walls, pos))
      (0, [], None) input
  in
  let bounds = (String.length (List.hd input), List.length input) in
  (obstacles, Option.get initial_coords, bounds)

let guard_rotate = function
  | UP -> RIGHT
  | DOWN -> LEFT
  | LEFT -> UP
  | RIGHT -> DOWN

let dir_name = function
  | UP -> "UP"
  | DOWN -> "DOWN"
  | LEFT -> "LEFT"
  | RIGHT -> "RIGHT"

let compute_offset (x, y) = function
  | UP -> (x, y - 1)
  | DOWN -> (x, y + 1)
  | LEFT -> (x - 1, y)
  | RIGHT -> (x + 1, y)

let in_bounds (x, y) (bound_x, bound_y) =
  x >= 0 && x < bound_x && y >= 0 && y < bound_y

let part1 input =
  let wallList, initial_pos, bounds = input in
  let walls = Hashtbl.create (List.length wallList) in
  List.iter (fun w -> Hashtbl.replace walls w 0) wallList;
  let visited = Hashtbl.create 10 in
  let rec walk state =
    Hashtbl.replace visited state.pos 0;
    let new_pos = compute_offset state.pos state.dir in
    if Hashtbl.mem walls new_pos then
      let new_dir = guard_rotate state.dir in
      (walk [@tailcall]) { state with dir = new_dir }
    else if in_bounds new_pos bounds then
      (walk [@tailcall]) { state with pos = new_pos }
  in
  walk { dir = UP; pos = initial_pos };
  Some (Hashtbl.length visited)

let part2 input =
  let wallList, initial_pos, bounds = input in
  let walls = Hashtbl.create (List.length wallList) in
  List.iter (fun w -> Hashtbl.replace walls w 0) wallList;

  let rec walk state test_wall visited =
    let new_pos = compute_offset state.pos state.dir in
    if Hashtbl.mem visited state then true
    else (
      Hashtbl.replace visited state 0;
      if 0 == compare new_pos test_wall || Hashtbl.mem walls new_pos then
        let new_dir = guard_rotate state.dir in
        (walk [@tailcall]) { state with dir = new_dir } test_wall visited
      else if in_bounds new_pos bounds then
        (walk [@tailcall]) { state with pos = new_pos } test_wall visited
      else false)
  in
  let original_visited = Hashtbl.create 10 in
  let already_known_wall =
    Hashtbl.to_seq_keys walls |> List.of_seq |> List.hd
  in
  let _ =
    walk { dir = UP; pos = initial_pos } already_known_wall original_visited
  in

  let new_wall = Hashtbl.create 0 in

  Seq.iter
    (fun pos ->
      if walk { dir = UP; pos = initial_pos } pos (Hashtbl.create 1) then
        let _ = Hashtbl.replace new_wall pos 0 in
        ())
    (Hashtbl.to_seq_keys original_visited |> Seq.map (fun s -> s.pos));
  Some (Hashtbl.length new_wall)

let run =
  (* Gonna run on test data for now since this is pretty brute force and slow...*)
  let input = read_lines "res/day6-test" |> parse in
  printf "Day 6\n";
  (match part1 input with
  | Some ans -> printf "Part 1: %d\n" ans
  | None -> print_endline "Part 1 is None");
  match part2 input with
  | Some ans -> printf "Part 2: %d\n" ans
  | None -> print_endline "Part 2 is None"
