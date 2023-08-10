open Printf

type instr = { num : int; src : int; dst : int }

(*Take a list and move it forward by 1*)
let cut = function [] -> [] | _ :: xs -> xs

(*Take a list and create an iter of tuples*)
let pair_iter lst =
  let a = List.map (fun x -> Some x) lst in
  let b = cut a in
  List.map2 (fun x y -> (x, y)) a (b @ [ None ])

let remove_runs lst =
  pair_iter lst
  |> List.filter_map (fun p ->
         match p with
         | Some a, Some b -> if String.compare a b == 0 then None else Some a
         | _ -> None)

let parse_instr ln =
  Scanf.sscanf ln "move %d from %d to %d" (fun num src dst -> { num; src; dst })

let explode str =
  let rec exp a b = if a < 0 then b else exp (a - 1) (str.[a] :: b) in
  exp (String.length str - 1) []

let parse_crates ln =
  let rec parse = function
    | a :: b :: _ :: _ :: xs -> (if a == '[' then Some b else None) :: parse xs
    | a :: b :: _ -> [ (if a == '[' then Some b else None) ]
    | _ -> [ None ]
  in
  ln |> explode |> parse

let input =
  let instrs = ref [] in
  (*Take advantage of the knowledge that we have 9 stacks*)
  let crates = Array.make 9 [] in
  Util.read_lines "res/day5"
  |> List.iter (fun ln ->
         (*We can safely ignore all the other lines*)
         if String.starts_with ~prefix:"move" ln then
           instrs := parse_instr ln :: !instrs
         else if String.starts_with ~prefix:"[" ln then
           let row_crates = parse_crates ln in
           for i = 0 to List.length row_crates - 1 do
             match List.nth row_crates i with
             | Some cur_crate -> crates.(i) <- crates.(i) @ [ cur_crate ]
             | None -> ()
           done);
  (!instrs, crates)

let rec last_item = function
  | [] -> None
  | [ x ] -> Some x
  | _ :: xs -> last_item xs

let print_crates crates =
  for i = 0 to Array.length crates - 1 do
    printf "%d " (i + 1);
    List.iter (fun x -> printf "[%c] " x) crates.(i);
    printf "\n"
  done

let print_instr instr =
  printf "move %d from %d to %d\n" instr.num instr.src instr.dst

let rec split_list n row =
  if n <= 0 then ([], row)
  else
    match row with
    | a :: lst ->
        let front, back = split_list (n - 1) lst in
        (a :: front, back)
    | a -> (a, [])

let part1 (instrs, crates) =
  let final_crates =
    List.rev instrs
    |> List.fold_left
         (fun crates instr ->
           let front, back = split_list instr.num crates.(instr.src - 1) in
           crates.(instr.dst - 1) <- List.rev front @ crates.(instr.dst - 1);
           crates.(instr.src - 1) <- back;
           crates)
         (Array.copy crates)
  in
  let top_row = ref [] in
  for i = 0 to Array.length final_crates - 1 do
    top_row := !top_row @ [ List.hd final_crates.(i) ]
  done;
  printf "\n";
  Some (String.of_seq (List.to_seq !top_row))

let part2 (instrs, crates) =
  let final_crates =
    List.rev instrs
    |> List.fold_left
         (fun crates instr ->
           let front, back = split_list instr.num crates.(instr.src - 1) in
           crates.(instr.dst - 1) <- front @ crates.(instr.dst - 1);
           crates.(instr.src - 1) <- back;
           crates)
         (Array.copy crates)
  in
  let top_row = ref [] in
  for i = 0 to Array.length final_crates - 1 do
    top_row := !top_row @ [ List.hd final_crates.(i) ]
  done;
  Some (String.of_seq (List.to_seq !top_row))

let run =
  printf "Day 5";
  let ans1 = part1 input in
  let ans2 = part2 input in
  let _ =
    match ans1 with
    | Some ans -> printf "Part 1: %s\n" ans
    | None -> print_endline "Part 1 is None"
  in
  match ans2 with
  | Some ans -> printf "Print 2: %s\n" ans
  | None -> print_endline "Part 2 is None"
