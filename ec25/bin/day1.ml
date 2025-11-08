type instr = Left of int | Right of int

let parse = function
  | [ names; _; instrs ] ->
      ( String.split_on_char ',' names,
        String.split_on_char ',' instrs
        |> List.map (fun i ->
            let dir = String.sub i 0 1 in
            let mag = String.sub i 1 (String.length i - 1) in
            match (dir, mag) with
            | "R", x -> Right (int_of_string x)
            | "L", x -> Left (int_of_string x)
            | _ -> raise (Invalid_argument "invalid input instruction")) )
  | _ -> raise (Invalid_argument "Invalid input file")

let part1 names instrs =
  let name_idx =
    List.fold_left
      (fun acc i ->
        match i with
        | Right x -> min (List.length names - 1) (x + acc)
        | Left x -> max 0 (acc - x))
      0 instrs
  in
  List.hd @@ Util.skip name_idx names

let part2 names instrs =
  let name_idx =
    List.fold_left
      (fun acc i ->
        match i with
        | Right x -> (x + acc) mod List.length names
        | Left x -> (List.length names - x + acc) mod List.length names)
      0 instrs
  in
  List.hd @@ Util.skip name_idx names

let string_of_instr = function
  | Right x -> Printf.sprintf "Right %d" x
  | Left x -> Printf.sprintf "Left %d" x

let part3 names instrs =
  let name_table =
    List.mapi (fun i name -> (i, name)) names |> List.to_seq |> Hashtbl.of_seq
  in
  List.iter
    (fun instr ->
      let idx =
        (match instr with
        | Right x -> x
        | Left x -> List.length names - (x mod List.length names)) mod List.length names
      in
      let new_top = Hashtbl.find name_table idx in
      let old_top = Hashtbl.find name_table 0 in
      Hashtbl.replace name_table 0 new_top;
      Hashtbl.replace name_table idx old_top;
      ())
    instrs;
  Hashtbl.find name_table 0

let run () =
  let names, instrs = Util.read_lines "res/day1.1" |> parse in
  Printf.printf "Part 1: %s\n" (part1 names instrs);
  let names, instrs = Util.read_lines "res/day1.2" |> parse in
  Printf.printf "Part 2: %s\n" (part2 names instrs);
  let names, instrs = Util.read_lines "res/day1.3" |> parse in
  Printf.printf "Part 3: %s\n" (part3 names instrs)
