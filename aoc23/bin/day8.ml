open Printf
open Util

type direction = L | R

exception NotDirection
exception ParseError

type map = {
  directions : direction list;
  mappings : (string, string * string) Hashtbl.t;
}

let parse lines =
  match List.filter (fun l -> String.length l != 0) lines with
  | directions :: xs ->
      let directions =
        String.to_seq directions |> List.of_seq
        |> List.map (function 'L' -> L | 'R' -> R | _ -> raise NotDirection)
      in
      let mappings = Hashtbl.create (List.length xs) in
      List.iter
        (fun ln ->
          match String.split_on_char ' ' ln with
          | [ frm; _; l; r ] ->
              Hashtbl.add mappings frm (String.sub l 1 3, String.sub r 0 3)
          | _ -> raise ParseError)
        xs;
      { directions; mappings }
  | _ -> raise ParseError

let traverse dir (mappings : ('string, 'string * 'string) Hashtbl.t) state =
  let left, right = Hashtbl.find mappings state in
  let nxt_state = if dir == R then right else left in
  nxt_state

let count_steps end_condition directions mappings init_state =
  let _end_state, dist =
    fold_left_until
      (fun (state, cnt) dir ->
        if end_condition state then None
        else
          let state = traverse dir mappings state in
          Some (state, cnt + 1))
      (init_state, 0) directions
  in
  dist

let part1 input =
  Some
    (count_steps (String.equal "ZZZ")
       (cycle_list input.directions)
       input.mappings "AAA")

let part2 input =
  Some
    (Hashtbl.to_seq_keys input.mappings
    |> Seq.filter (String.ends_with ~suffix:"A")
    |> Seq.map
         (count_steps
            (String.ends_with ~suffix:"Z")
            (cycle_list input.directions)
            input.mappings)
    |> Seq.fold_left lcm 1)

let run =
  let input = read_lines "res/day8" |> parse in
  printf "Day 8\n";
  (match part1 input with
  | Some ans -> printf "Part 1: %d\n" ans
  | None -> print_endline "Part 1 is None");
  match part2 input with
  | Some ans -> printf "Part 2: %d\n\n" ans
  | None -> print_endline "Part 2 is None"
