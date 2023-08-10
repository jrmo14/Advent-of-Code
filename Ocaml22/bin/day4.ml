open Printf

type id_range = { start : int; finish : int }

let range_of_str s =
  match String.split_on_char '-' s with
  | [ s; f ] -> Some { start = int_of_string s; finish = int_of_string f }
  | _ -> None

type elf_pair = { a : id_range; b : id_range }

let contained p =
  (p.a.start <= p.b.start && p.a.finish >= p.b.finish)
  || (p.b.start <= p.a.start && p.b.finish >= p.a.finish)

let overlap p = p.a.start <= p.b.finish && p.b.start <= p.a.finish

let input =
  Util.read_lines "res/day4"
  |> List.filter_map (fun ln ->
         match String.split_on_char ',' ln with
         | [ str_a; str_b ] -> (
             let range_a = range_of_str str_a in
             let range_b = range_of_str str_b in
             match (range_a, range_b) with
             | Some a, Some b -> Some { a; b }
             | _ -> None)
         | _ -> None)

let part1 input =
  Some
    (input
    |> List.map (fun e -> if contained e then 1 else 0)
    |> List.fold_left ( + ) 0)

let part2 input =
  Some
    (input
    |> List.map (fun e -> if overlap e then 1 else 0)
    |> List.fold_left ( + ) 0)

let run =
  printf "Day 4\n";
  let ans1 = part1 input in
  let ans2 = part2 input in
  let _ =
    match ans1 with
    | Some ans -> printf "Part 1: %d\n" ans
    | None -> print_endline "Part 1 is None"
  in
  match ans2 with
  | Some ans -> printf "Part 2: %d\n" ans
  | None -> print_endline "Part 2 is None"
