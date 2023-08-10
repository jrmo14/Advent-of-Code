open Printf

let prio itm =
  match itm with
  | 'A' .. 'Z' -> int_of_char itm - int_of_char 'A' + 27
  | 'a' .. 'z' -> int_of_char itm - int_of_char 'a' + 1
  | _ -> 0

let find_idx n =
  let rec loop i =
    if i >= 64 then raise Not_found
    else if
      let mask = 1 lsl i in
      n land mask == mask
    then i
    else loop (i + 1)
  in
  loop 0

let input =
  Util.read_lines "res/day3"
  |> List.map (fun ln ->
         let len = String.length ln / 2 in
         (String.sub ln 0 len, String.sub ln len len))

let part1 input =
  Some
    (input
    |> List.map (fun (a, b) ->
           let mask_a =
             String.fold_left (fun mask c -> mask lor (1 lsl prio c)) 0 a
           in
           let mask_b =
             String.fold_left (fun mask c -> mask lor (1 lsl prio c)) 0 b
           in
           find_idx (mask_a land mask_b))
    |> List.fold_left ( + ) 0)

let group_badge (a1, a2) (b1, b2) (c1, c2) =
  let a_items =
    String.fold_left (fun mask i -> mask lor (1 lsl prio i)) 0 (a1 ^ a2)
  in
  let b_items =
    String.fold_left (fun mask i -> mask lor (1 lsl prio i)) 0 (b1 ^ b2)
  in
  let c_items =
    String.fold_left (fun mask i -> mask lor (1 lsl prio i)) 0 (c1 ^ c2)
  in
  find_idx (a_items land b_items land c_items)

let part2 input =
  let rec loop = function
    | [ a; b; c ] -> group_badge a b c
    | a :: b :: c :: lst -> group_badge a b c + loop lst
    | _ -> 0
  in
  Some (loop input)

let run =
  printf "Day 3\n";
  let ans1 = part1 input in
  let ans2 = part2 input in
  let _ =
    match ans1 with
    | Some ans -> printf "Part 1: %d\n" ans
    | None -> print_endline "Part 1 is None"
  in
  match ans2 with
  | Some ans -> printf "Print 2: %d\n" ans
  | None -> print_endline "Part 2 is None"
