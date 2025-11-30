open Printf
open Util

let parse input =
  match group_by (fun line -> 0 != String.length line) input with
  | [ map; commands ] ->
      let map = List.filter (fun x -> 0 != String.length x) map in
      let cmds = List.fold_left (fun acc line -> line ^ acc) "" commands in
      (map, cmds)
  | _ -> raise (Invalid_argument "Parsing failed")

let print_map map =
  Array.iter
    (fun row ->
      Array.iter (printf "%c") row;
      printf "\n")
    map;
  printf "\n"

let apply_command do_move map position cmd =
  let offset =
    match cmd with
    | '^' -> (0, -1)
    | 'v' -> (0, 1)
    | '>' -> (1, 0)
    | '<' -> (-1, 0)
    | _ -> raise (Invalid_argument (sprintf "Unknown direction %c" cmd))
  in
  let did_move = do_move map offset position false in
  if did_move then (
    let nx, ny = point_add position offset in
    map.(ny).(nx) <- '@';
    (nx, ny))
  else position

let find_sub map =
  let _, pos =
    Array.fold_left
      (fun (y, pos) row ->
        let _, pos =
          Array.fold_left
            (fun (x, pos) e ->
              if e == '@' then (x + 1, Some (x, y)) else (x + 1, pos))
            (0, pos) row
        in
        (y + 1, pos))
      (0, None) map
  in
  pos

let part1 (map, cmds) =
  let map =
    map |> List.to_seq
    |> Seq.map (fun line -> String.to_seq line |> Array.of_seq)
    |> Array.of_seq
  in
  let pos =
    match find_sub map with
    | Some p -> p
    | None -> raise (Invalid_argument "No position in map")
  in
  let apply pos cmd =
    let rec do_move map offset pos pushing =
      let nx, ny = point_add pos offset in
      let can_move =
        match map.(ny).(nx) with
        | '#' -> false
        | 'O' -> do_move map offset (nx, ny) true
        | '.' -> true
        | c -> raise (Invalid_argument (sprintf "Unknown character: %c" c))
      in
      if can_move then (
        let x, y = pos in
        map.(y).(x) <- '.';
        if pushing then map.(ny).(nx) <- 'O');
      can_move
    in
    (* printf "Move: %c\n" b; *)
    let x = apply_command do_move map pos cmd in
    (* print_map map; *)
    x
  in
  let _ = String.to_seq cmds |> Seq.fold_left apply pos in
  let _, sum =
    Array.fold_left
      (fun (y, acc) row ->
        let _, acc =
          Array.fold_left
            (fun (x, acc) e ->
              if e == 'O' then (x + 1, acc + (100 * y) + x) else (x + 1, acc))
            (0, acc) row
        in
        (y + 1, acc))
      (0, 0) map
  in
  Some sum

let part2 (_map, _cmds) = None
  (* let map = *)
    (* map |> List.to_seq *)
    (* |> Seq.map (fun line -> *)
           (* String.to_seq line *)
           (* |> Seq.fold_left *)
                (* (fun acc e -> *)
                  (* acc *)
                  (* ^ *)
                  (* match e with *)
                  (* | '#' -> "##" *)
                  (* | 'O' -> "[]" *)
                  (* | '.' -> ".." *)
                  (* | '@' -> "@." *)
                  (* | _ -> raise (Invalid_argument "Cannot expand character")) *)
                (* "" *)
           (* |> String.to_seq |> Array.of_seq) *)
    (* |> Array.of_seq *)
  (* in *)
  (* let pos = *)
    (* match find_sub map with *)
    (* | Some p -> p *)
    (* | None -> raise (Invalid_argument "No position in map") *)
  (* in *)
  (* let apply pos cmd = *) 
    (* let rec do_move map offset pos pushing = *)
      (* let nxt = Hashtbl.to_seq_keys pushing |> Seq.map (fun p -> point_add offset p) in *)
    
    
    
    
    (* pos  in *)

  (* let _ = String.to_seq cmds |> Seq.fold_left apply pos in *)
  (* let _, sum = *)
    (* Array.fold_left *)
      (* (fun (y, acc) row -> *)
        (* let _, acc = *)
          (* Array.fold_left *)
            (* (fun (x, acc) e -> *)
              (* if e == '[' then (x + 1, acc + (100 * y) + x) else (x + 1, acc)) *)
            (* (0, acc) row *)
        (* in *)
        (* (y + 1, acc)) *)
      (* (0, 0) map *)
  (* in *)
  (* Some sum *)

let run () =
  let input = read_lines "res/day15" |> parse in
  printf "Day 15\n";
  (match part1 input with
  | Some ans -> printf "Part 1: %d\n" ans
  | None -> print_endline "Part 1 is None");
  match part2 input with
  | Some ans -> printf "Part 2: %d\n" ans
  | None -> print_endline "Part 2 is None"
