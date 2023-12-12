open Printf

type card = { idx : int; nums : int list; winners : int list }

let compare x y = if x > y then 1 else if x < y then -1 else 0

let parse_card line =
  match String.split_on_char '|' line with
  | [ before; nums ] ->
      let my_nums =
        nums |> String.split_on_char ' '
        |> List.filter (fun s -> String.length s != 0)
        |> List.map int_of_string
      in
      let before =
        before |> String.split_on_char ' ' |> List.tl
        |> List.filter (fun s -> String.length s != 0)
      in
      let idx = List.hd before in
      let idx = int_of_string (String.sub idx 0 (String.length idx - 1)) in
      let winners = List.tl before |> List.map int_of_string in
      Some
        {
          idx;
          nums = List.sort compare my_nums;
          winners = List.sort compare winners;
        }
  | _ -> None

let count_wins card =
  let rec inner nums winners =
    match nums with
    | x :: t -> (
        match winners with
        | y :: c ->
            if x > y then inner (x :: t) c
            else if x < y then inner t (y :: c)
            else 1 + inner t (y :: c)
        | _ -> 0)
    | _ -> 0
  in
  inner card.nums card.winners

(*https://en.wikipedia.org/wiki/Exponentiation_by_squaring*)
(*With the change to negative exponents since this is for integers*)
let rec pow base exp =
  if exp < 0 then 0
  else if exp == 0 then 1
  else if exp mod 2 == 0 then pow (base * base) (exp / 2)
  else base * pow (base * base) ((exp - 1) / 2)

let input = Util.read_lines "res/day4-test" |> List.filter_map parse_card

let part1 input =
  Some
    (input
    |> List.map (fun card -> pow 2 (count_wins card - 1))
    |> List.fold_left ( + ) 0)

type winning_card = { win_idx : int; num_wins : int }

let rec take n lst =
  if n > 0 then match lst with x :: t -> x :: take (n - 1) t | [] -> []
  else []

let rec skip n lst =
  if n > 0 then match lst with _ :: t -> skip (n - 1) t | [] -> [] else lst

(*let rec count_cards acc cards =
  match cards with
  | x :: t -> 1 + count_cards (take x.num_wins (List.tl cards)) + count_cards t
  | [] -> 0
*)
let part2 _input = None
(* Some
   (input
   |> List.map (fun c -> { win_idx = c.idx; num_wins = count_wins c })
   |> count_cards)
*)

let run =
  printf "Day 4\n";
  (match part1 input with
  | Some ans -> printf "Part 1: %d\n" ans
  | None -> print_endline "Part 1 is None");
  match part2 input with
  | Some ans -> printf "Part 2: %d\n\n" ans
  | None -> print_endline "Part 2 is None\n"
