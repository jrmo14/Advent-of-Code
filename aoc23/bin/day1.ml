open Printf

let input = Util.read_lines "res/day1-test2"

let rec get_digits s =
  match s with
  | '1' :: t | 'o' :: 'n' :: 'e' :: t -> [ 1 ] @ get_digits t
  | '2' :: t | 't' :: 'w' :: 'o' :: t -> [ 2 ] @ get_digits t
  | '3' :: t | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: t -> [ 3 ] @ get_digits t
  | '4' :: t | 'f' :: 'o' :: 'u' :: 'r' :: t -> [ 4 ] @ get_digits t
  | '5' :: t | 'f' :: 'i' :: 'v' :: 'e' :: t -> [ 5 ] @ get_digits t
  | '6' :: t | 's' :: 'i' :: 'x' :: t -> [ 6 ] @ get_digits t
  | '7' :: t | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: t -> [ 7 ] @ get_digits t
  | '8' :: t | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: t -> [ 8 ] @ get_digits t
  | '9' :: t | 'n' :: 'i' :: 'n' :: 'e' :: t -> [ 9 ] @ get_digits t
  | '0' :: t | 'z' :: 'e' :: 'r' :: 'o' :: t -> [ 0 ] @ get_digits t
  | _ :: t -> get_digits t
  | [] -> []

let part1 input =
  Some
    (input
    |> List.fold_left
         (fun total line ->
           let digits =
             line |> String.to_seq |> List.of_seq
             |> List.filter_map (fun c ->
                    if c >= '0' && c <= '9' then Some (Char.code c - 48)
                    else None)
           in
           (10 * List.hd digits) + List.hd (List.rev digits) + total)
         0)

let part2 input =
  Some
    (input
    |> List.fold_left
         (fun total line ->
           let digits = line |> String.to_seq |> List.of_seq |> get_digits in
           (10 * List.hd digits) + List.hd (List.rev digits) + total)
         0)

let run =
  printf "Day 1\n";
(*  let ans1 = part1 input in
  let _ =
    match ans1 with
    | Some ans -> printf "Part 1: %d\n" ans
    | None -> print_endline "Part 1 is None"
  in
*)  let ans2 = part2 input in
  match ans2 with
  | Some ans -> printf "Part 2: %d\n" ans
  | None -> print_endline "Part 2 is None"
