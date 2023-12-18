open Printf
open Util

type card = A | K | Q | J | T | Number of int

exception NotCard
exception NotHand

let parse_card = function
  | 'A' -> A
  | 'K' -> K
  | 'Q' -> Q
  | 'J' -> J
  | 'T' -> T
  | x when x >= '2' && x <= '9' -> Number (int_of_char x - int_of_char '0')
  | _ -> raise NotCard

let int_of_card = function
  | A -> 14
  | K -> 13
  | Q -> 12
  | J -> 11
  | T -> 10
  | Number v -> v

type hand_type =
  | FiveOKind
  | FourOKind
  | FullHouse
  | ThreeOKind
  | TwoPair
  | Pair
  | High

let string_of_hand_type = function
  | FiveOKind -> "FiveOKind"
  | FourOKind -> "FourOKind"
  | FullHouse -> "FullHouse"
  | ThreeOKind -> "ThreeOKind"
  | TwoPair -> "TwoPair"
  | Pair -> "Pair"
  | High -> "High"

let int_of_hand_type h =
  match h with
  | FiveOKind -> 7
  | FourOKind -> 6
  | FullHouse -> 5
  | ThreeOKind -> 4
  | TwoPair -> 3
  | Pair -> 2
  | High -> 1

(*
   Take a sorted list and break into homogeneous chunks
   TTKAA -> [TT] [K] [AA]
*)
let chunk_seqs lst =
  let rec inner (cur_chunk, acc) lst =
    match lst with
    | Number x :: Number y :: rem when x == y ->
        inner (Number x :: cur_chunk, acc) (Number y :: rem)
    | x :: y :: rem when x == y -> inner (x :: cur_chunk, acc) (y :: rem)
    | x :: y :: rem when x != y -> inner ([], (x :: cur_chunk) :: acc) (y :: rem)
    | [ x ] -> (x :: cur_chunk) :: acc
    | _ -> raise NotHand
  in
  inner ([], []) lst

let compute_hand_type cards =
  let chunked_seqs =
    List.sort List.compare_lengths
      (chunk_seqs (List.sort (fun a b -> int_of_card a - int_of_card b) cards))
    |> List.rev
  in
  match chunked_seqs with
  | [ _; _; _; _; _ ] -> High
  | [ _ ] -> FiveOKind
  | [ a; _b ] -> if List.length a == 4 then FourOKind else FullHouse
  | [ a; _; _ ] -> if List.length a == 3 then ThreeOKind else TwoPair
  | _ -> Pair

let compare_hands a b =
  let order =
    int_of_hand_type (compute_hand_type a)
    - int_of_hand_type (compute_hand_type b)
  in
  if order == 0 then
    match
      zip a b
      |> List.fold_left
           (fun acc (a, b) ->
             match acc with
             | Some x -> Some x
             | None ->
                 let c = int_of_card a - int_of_card b in
                 if c == 0 then None else Some c)
           None
    with
    | Some x -> x
    | None -> 0
  else order

type hand = { cards : card list; bid : int }

let parse_hand line =
  match String.split_on_char ' ' line with
  | [ cards; bid ] ->
      let cards =
        cards |> String.to_seq |> List.of_seq |> List.map parse_card
      in
      let bid = int_of_string bid in
      { cards; bid }
  | _ -> raise NotHand

let char_of_card = function
  | A -> 'A'
  | K -> 'K'
  | Q -> 'Q'
  | J -> 'J'
  | T -> 'T'
  | Number x -> char_of_int (x + int_of_char '0')

let string_of_cards cards =
  cards |> List.map char_of_card |> List.to_seq |> String.of_seq

let print_hand hand hand_type_compute =
  let cards_str = hand.cards |> string_of_cards in
  let ht = hand_type_compute hand in
  printf "%s %s\n" cards_str (string_of_hand_type ht)

let input = read_lines "res/day7" |> List.map parse_hand

let rec print_hands hand_type_compute hands =
  match hands with
  | x :: xs ->
      print_hand x hand_type_compute;
      x :: print_hands hand_type_compute xs
  | [] ->
      printf "\n";
      []

let part1 input =
  Some
    (List.sort (fun a b -> compare_hands a.cards b.cards) input
    |> List.mapi (fun idx this_hand -> (1 + idx) * this_hand.bid)
    |> List.fold_left ( + ) 0)

let compute_hand_type_jokers cards =
  let tbl = Hashtbl.create 14 in
  List.iter
    (fun c ->
      Hashtbl.replace tbl c
        ((match Hashtbl.find_opt tbl c with Some a -> a | None -> 0) + 1))
    cards;
  let joker_count =
    match Hashtbl.find_opt tbl J with Some c -> c | None -> 0
  in
  let chunks =
    Hashtbl.to_seq tbl |> List.of_seq
    |> List.filter (fun c -> match c with J, _ -> false | _ -> true)
    |> List.sort (fun (_, a) (_, b) -> compare b a)
  in
  (*List.iter (fun (crd, cnt) -> printf "%c %d\n" (char_of_card crd) cnt) chunks;*)
  let sans_joker_type =
    match chunks with
    | (_, 5) :: _ -> FiveOKind
    | (_, 4) :: _ -> FourOKind
    | (_, 3) :: (_, 2) :: _ -> FullHouse
    | (_, 3) :: _ -> ThreeOKind
    | (_, 2) :: (_, 2) :: _ -> TwoPair
    | (_, 2) :: _ -> Pair
    | [] -> FiveOKind
    | _ -> High
  in
  match sans_joker_type with
  | FiveOKind -> FiveOKind
  | FourOKind -> if joker_count == 1 then FiveOKind else FourOKind
  | FullHouse -> FullHouse
  | ThreeOKind ->
      if joker_count == 2 then FiveOKind
      else if joker_count == 1 then FourOKind
      else ThreeOKind
  | TwoPair -> if joker_count == 1 then FullHouse else TwoPair
  | Pair ->
      if joker_count == 3 then FiveOKind
      else if joker_count == 2 then FourOKind
      else if joker_count == 1 then ThreeOKind
      else Pair
  | High ->
      if joker_count == 4 then FiveOKind
      else if joker_count == 3 then FourOKind
      else if joker_count == 2 then ThreeOKind
      else if joker_count == 1 then Pair
      else High

let int_of_card_joker = function
  | A -> 14
  | K -> 13
  | Q -> 12
  | J -> 0
  | T -> 10
  | Number v -> v

let compare_hands_jokers a b =
  let order =
    int_of_hand_type (compute_hand_type_jokers a)
    - int_of_hand_type (compute_hand_type_jokers b)
  in
  if order == 0 then
    match
      zip a b
      |> List.fold_left
           (fun acc (a, b) ->
             match acc with
             | Some x -> Some x
             | None ->
                 let c = int_of_card_joker a - int_of_card_joker b in
                 if c == 0 then None else Some c)
           None
    with
    | Some x -> x
    | None -> 0
  else order

let part2 input =
  Some
    (List.sort (fun a b -> compare_hands_jokers a.cards b.cards) input
    |> List.mapi (fun idx this_hand -> (1 + idx) * this_hand.bid)
    |> List.fold_left ( + ) 0)

let run =
  printf "Day 7\n";
  (match part1 input with
  | Some ans -> printf "Part 1: %d\n" ans
  | None -> print_endline "Part 1 is None");
  match part2 input with
  | Some ans -> printf "Part 2: %d\n" ans
  | None -> print_endline "Part 2 is None"
