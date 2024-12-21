open Printf
open Util

type robot = { pos : int * int; vel : int * int }
let bounds = (101, 103)
(* let bounds = (11, 7) *)
let input_file = "res/day14"

let parse_robot line =
  let parse_tup s =
    let s = String.sub s 2 (String.length s - 2) in
    match String.split_on_char ',' s with
    | [ x; y ] -> (int_of_string x, int_of_string y)
    | _ -> raise (Invalid_argument "can't parse tuple")
  in
  let p, v =
    match String.split_on_char ' ' line with
    | [ p; v ] -> (p, v)
    | _ -> raise (Invalid_argument "can't parse line")
  in
  { pos = parse_tup p; vel = parse_tup v }

let wrap_pos (bx, by) (px, py) =
  let px = if px < 0 then bx + px else px in
  let py = if py < 0 then by + py else py in
  let px = if px >= bx then px - bx else px in
  let py = if py >= by then py - by else py in
  (px, py)

let get_quad_idx (bx, by) (px, py) =
  if px < bx / 2 then
    if py < by / 2 then Some 0 else if py > by / 2 then Some 1 else None
  else if px > bx / 2 then
    if py < by / 2 then Some 2 else if py > by / 2 then Some 3 else None
  else None

let step_robot r = { r with pos = wrap_pos bounds (point_add r.pos r.vel) }

let rec run_robot r s =
  if s == 0 then r else (run_robot [@tailcall]) (step_robot r) (s - 1)

let parse input = List.map parse_robot input
let step_robots robots = List.map step_robot robots

let part1 input =
  let final_robots =
    List.fold_left (fun robots _ -> step_robots robots) input (0 -- 99)
  in
  Some
    (List.fold_left
       (fun acc r ->
         match get_quad_idx bounds r.pos with
         | Some i ->
             acc.(i) <- acc.(i) + 1;
             acc
         | None -> acc)
       [| 0; 0; 0; 0 |] final_robots
    |> Array.fold_left ( * ) 1)

let is_tree robots =
  let mat = Array.init_matrix 101 103 (fun _ _ -> false) in
  List.iter
    (fun r ->
      let px, py = r.pos in
      mat.(px).(py) <- true)
    robots;
  let bottom =
    any
      (fun x -> x > 10)
      (List.map
         (fun x ->
           List.fold_left
             (fun acc y -> if mat.(x).(y) then acc + 1 else 0)
             0 (50 -- 102))
         (0 -- 100))
  in
  List.iter
    (fun y ->
      List.iter
        (fun x -> if mat.(x).(y) then printf "#" else printf ".")
        (0 -- 100);
      printf "\n")
    (0 -- 102);
  printf "\n\n";
  bottom

let part2 input =
  Some
    (let _r, idx =
       fold_left_until
         (fun (robots, idx) _n ->
         printf "ITER: %d\n" idx;
           let robots = step_robots robots in
           if is_tree robots then None else Some (robots, idx + 1))
         (input, 0) ((0--24652) |> List.to_seq)
     in
     idx)


let run () =
  let input = read_lines input_file |> parse in
  printf "Day 14\n";
  (match part1 input with
  | Some ans -> printf "Part 1: %d\n" ans
  | None -> print_endline "Part 1 is None");
  match part2 input with
  | Some ans -> printf "Part 2: %d\n" ans
  | None -> print_endline "Part 2 is None"
