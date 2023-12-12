open Printf

let input = Util.read_lines "res/day1"

(*Need to keep the last character when we consume a token so we can parse eighthree as 83*)
let rec get_digits s =
  match s with
  | '1' :: t -> 1 :: get_digits t
  | 'o' :: 'n' :: x :: t when x = 'e' -> 1 :: get_digits (x :: t)
  | '2' :: t -> 2 :: get_digits t
  | 't' :: 'w' :: x :: t when x = 'o' -> 2 :: get_digits (x :: t)
  | '3' :: t -> 3 :: get_digits t
  | 't' :: 'h' :: 'r' :: 'e' :: x :: t when x = 'e' -> 3 :: get_digits (x :: t)
  | '4' :: t -> 4 :: get_digits t
  | 'f' :: 'o' :: 'u' :: x :: t when x = 'r' -> 4 :: get_digits (x :: t)
  | '5' :: t -> 5 :: get_digits t
  | 'f' :: 'i' :: 'v' :: x :: t when x = 'e' -> 5 :: get_digits (x :: t)
  | '6' :: t -> 6 :: get_digits t
  | 's' :: 'i' :: x :: t when x = 'x' -> 6 :: get_digits (x :: t)
  | '7' :: t -> 7 :: get_digits t
  | 's' :: 'e' :: 'v' :: 'e' :: x :: t when x = 'n' -> 7 :: get_digits (x :: t)
  | '8' :: t -> 8 :: get_digits t
  | 'e' :: 'i' :: 'g' :: 'h' :: x :: t when x = 't' -> 8 :: get_digits (x :: t)
  | '9' :: t -> 9 :: get_digits t
  | 'n' :: 'i' :: 'n' :: x :: t when x = 'e' -> 9 :: get_digits (x :: t)
  | '0' :: t -> 0 :: get_digits t
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
  let ans1 = part1 input in
  let _ =
    match ans1 with
    | Some ans -> printf "Part 1: %d\n" ans
    | None -> print_endline "Part 1 is None"
  in
  let ans2 = part2 input in
  match ans2 with
  | Some ans -> printf "Part 2: %d\n\n" ans
  | None -> print_endline "Part 2 is None"
