open Printf
open Util

let parse input =
  List.hd input |> String.to_seq
  |> Seq.map (fun c -> int_of_char c - int_of_char '0')
  |> List.of_seq

let build_sectors lst =
  let chunks = chunk 2 lst in
  let sectors, _ =
    List.fold_left
      (fun (acc, idx) -> function
        | [ live; empty ] -> ((empty, None) :: (live, Some idx) :: acc, idx + 1)
        | [ live ] -> ((live, Some idx) :: acc, idx + 1)
        | _ -> raise (Invalid_argument "Chunk didn't produce 2 or 1 elements"))
      ([], 0) chunks
  in
  Array.of_list (List.rev sectors |> List.filter (fun (x, _) -> x > 0))

let compute_range offset size =
  offset -- (size + offset - 1) |> List.fold_left ( + ) 0

let part1 input =
  let sectors = build_sectors input in
  let checksum_update chk id off size =
    (* List.iter (fun _ -> printf "%d" id) (0 -- (size - 1)); *)
    chk + (id * compute_range off size)
  in

  let rec compute_sector_checksum front back off chk =
    if front <= back then
      match sectors.(front) with
      | front_size, Some front_id ->
          let new_checksum = checksum_update chk front_id off front_size in
          let new_off = off + front_size in
          (compute_sector_checksum [@tailcall]) (front + 1) back new_off
            new_checksum
      | front_size, None -> (
          match sectors.(back) with
          | back_size, Some back_id ->
              if front_size > back_size then (
                let new_checksum = checksum_update chk back_id off back_size in
                let new_off = off + back_size in
                sectors.(front) <- (front_size - back_size, None);
                (compute_sector_checksum [@tailcall]) front (back - 1) new_off
                  new_checksum)
              else if back_size > front_size then (
                let new_checksum = checksum_update chk back_id off front_size in
                let new_off = off + front_size in
                sectors.(back) <- (back_size - front_size, Some back_id);
                (compute_sector_checksum [@tailcall]) (front + 1) back new_off
                  new_checksum)
              else
                let new_checksum = checksum_update chk back_id off back_size in
                let new_off = off + front_size in
                (compute_sector_checksum [@tailcall]) (front + 1) (back - 1)
                  new_off new_checksum
          | _, None ->
              (compute_sector_checksum [@tailcall]) front (back - 1) off chk)
    else (*printf"\n";*) chk
  in
  Some (compute_sector_checksum 0 (Array.length sectors - 1) 0 0)

let part2 input =
  let sectors = build_sectors input in

  let back = Array.length sectors - 1 in

  let find_victim_sector empty_size =
    let rec inner empty_size idx =
      if idx > 0 then
        match sectors.(idx) with
        | sect_size, Some _ when empty_size >= sect_size -> Some idx
        | _ -> (inner [@tailcall]) empty_size (idx - 1)
      else None
    in
    let x = inner empty_size back in
    x
  in

  let checksum_update chk id off size =
    (* List.iter (fun _ -> printf "%d" id) (0 -- (size - 1)); *)
    chk + (id * compute_range off size)
  in

  let rec compact_sectors front off chk =
    if front < back then
      match sectors.(front) with
      | front_size, Some front_id ->
          (compact_sectors [@tailcall]) (front + 1) (off + front_size)
            (checksum_update chk front_id off front_size)
      | front_size, None -> (
          match find_victim_sector front_size with
          | Some back_idx when back_idx > front -> (
              match sectors.(back_idx) with
              | back_size, Some back_id ->
                  (* printf "replaced sector count %d\n" (Hashtbl.length replaced); *)
                  sectors.(back_idx) <- (back_size, None);
                  sectors.(front) <- (front_size - back_size, None);
                  (compact_sectors [@tailcall]) front (off + back_size)
                    (checksum_update chk back_id off back_size)
              | _ ->
                  raise (Invalid_argument "Found victim sector with null value")
              )
          | _ ->
              (compact_sectors [@tailcall]) (front + 1) (off + front_size) chk)
    else chk
  in
  Some (compact_sectors 0 0 0)

let run =
  let input = read_lines "res/day9" |> parse in
  printf "Day 9\n";
  (match part1 input with
  | Some ans -> printf "Part 1: %d\n" ans
  | None -> print_endline "Part 1 is None");
  match part2 input with
  | Some ans -> printf "Part 2: %d\n" ans
  | None -> print_endline "Part 2 is None"
