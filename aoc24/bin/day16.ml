open Printf
open Util

let parse input =
  let map =
    input
    |> List.map (fun line -> String.to_seq line |> Array.of_seq)
    |> Array.of_list
  in
  let _, pos =
    Array.fold_left
      (fun (y, pos) row ->
        let _, pos =
          Array.fold_left
            (fun (x, pos) e ->
              if e == 'S' then (x + 1, Some (x, y)) else (x + 1, pos))
            (0, pos) row
        in
        (y + 1, pos))
      (0, None) map
  in
  let pos =
    match pos with
    | Some p -> p
    | None -> raise (Invalid_argument "No starting position")
  in
  (map, pos)

let part1 (map, pos) =
  let visited = Hashtbl.create 0 in
  let is_wall (x, y) = map.(y).(x) == '#' in
  let rec visit = function
    | [] -> None
    | xs ->
        List.iter (fun (p, _, _) -> Hashtbl.replace visited p 0) xs;

        let next_moves =
          List.fold_left
            (fun acc (p, dir, score) ->
              let n = array_move_dir p dir in
              if not (is_wall n) then (n, dir, score + 1) :: acc
              else
                let cw_can = array_move_dir p (cw_rotate dir) in
                let ccw_can = array_move_dir p (ccw_rotate dir) in
                let acc =
                  if not (is_wall cw_can) then
                    (cw_can, cw_rotate dir, score + 1000) :: acc
                  else acc
                in
                if not (is_wall ccw_can) then
                  (ccw_can, ccw_rotate dir, score + 1000) :: acc
                else acc)
            [] xs
        in
        let finished = List.filter (fun (p, _, score) -> )
        visit next_moves
  in
  let x = visit [ pos ] in

  None

let part2 _input = None

let run () =
  let input = read_lines "res/day16" |> parse in
  printf "Day 16\n";
  (match part1 input with
  | Some ans -> printf "Part 1: %d\n" ans
  | None -> print_endline "Part 1 is None");
  match part2 input with
  | Some ans -> printf "Part 2: %d\n" ans
  | None -> print_endline "Part 2 is None"
