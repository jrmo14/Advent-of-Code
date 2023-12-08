open Printf

type cell = Number of int | AdjacentNumber of int | Symbol of char | Void

let input =
  Util.read_lines "res/day3"
  |> List.map (fun ln ->
         ln |> String.to_seq |> List.of_seq
         |> List.map (function
              | x when x >= '0' && x <= '9' ->
                  Number (int_of_char x - int_of_char '0')
              | '.' -> Void
              | c -> Symbol c))

(* The [@tailcall] is to make sure that the inner function is in fact tail recursive *)
let fold_in_diagonals merge_rows cells =
  let empty_row = List.init (List.length (List.hd cells)) (fun _ -> Void) in
  let rec inner = function
    | x :: y :: z :: t -> merge_rows x y z :: inner (y :: z :: t)
    | [ y; z ] -> [ merge_rows y z empty_row ]
    | _ -> []
  in
  (inner [@tailcall]) (empty_row :: cells)

(* Part 1 specific code *)
let merge_rows_part1 a b c =
  List.combine a (List.combine b c)
  |> List.map (function
       | Symbol _, (Number x, _) | _, (Number x, Symbol _) -> AdjacentNumber x
       | _, (Number x, _) -> Number x
       | Symbol c, (_, _) | _, (Symbol c, _) | _, (_, Symbol c) -> Symbol c
       | _ -> Void)

type part1_state = KnownValid of int | PossibleValid of int | Invalid
type part1_accum = { count : int; state : part1_state }

let state_machine_part1 acc c =
  match c with
  | Symbol _ ->
      {
        count =
          (acc.count
          +
          match acc.state with
          | KnownValid v | PossibleValid v -> v
          | Invalid -> 0);
        state = KnownValid 0;
      }
  | AdjacentNumber x ->
      {
        count = acc.count;
        state =
          (match acc.state with
          | KnownValid v | PossibleValid v -> KnownValid ((v * 10) + x)
          | Invalid -> KnownValid x);
      }
  | Number x ->
      {
        count = acc.count;
        state =
          (match acc.state with
          | KnownValid v -> KnownValid ((v * 10) + x)
          | PossibleValid v -> PossibleValid ((v * 10) + x)
          | Invalid -> PossibleValid x);
      }
  | Void ->
      {
        count = (acc.count + match acc.state with KnownValid v -> v | _ -> 0);
        state = Invalid;
      }

let part1 input =
  Some
    (input
    |> fold_in_diagonals merge_rows_part1
    |> List.map (fun row ->
           let final =
             List.fold_left state_machine_part1
               { count = 0; state = Invalid }
               row
           in
           final.count + match final.state with KnownValid x -> x | _ -> 0)
    |> List.fold_left ( + ) 0)

(* Part 2 specific code *)
type coord = { ridx : int; cidx : int }
type part = { loc : coord; length : int; value : int }
type part2_accum = { gears : coord list; parts : part list }

let handle_row ridx row =
  let rec inner acc cur cidx r =
    match r with
    | Symbol '*' :: t ->
        inner
          {
            gears = { ridx; cidx } :: acc.gears;
            parts =
              (match cur with Some v -> v :: acc.parts | None -> acc.parts);
          }
          None (cidx + 1) t
    | Number x :: t ->
        inner
          { gears = acc.gears; parts = acc.parts }
          (match cur with
          | Some v ->
              Some
                {
                  loc = v.loc;
                  length = v.length + 1;
                  value = (v.value * 10) + x;
                }
          | None -> Some { loc = { ridx; cidx }; length = 1; value = x })
          (cidx + 1) t
    | _ :: t ->
        inner
          {
            gears = acc.gears;
            parts =
              (match cur with Some v -> v :: acc.parts | None -> acc.parts);
          }
          None (cidx + 1) t
    | _ ->
        {
          gears = acc.gears;
          parts =
            (match cur with Some v -> v :: acc.parts | None -> acc.parts);
        }
  in
  (inner [@tailcall]) { gears = []; parts = [] } None 0 row

let touching g p =
  (g.cidx >= p.loc.cidx - 1 && g.cidx <= p.loc.cidx + p.length)
  && abs (g.ridx - p.loc.ridx) <= 1

let part2 input =
  let locations =
    input |> List.mapi handle_row
    |> List.fold_left
         (fun a b -> { gears = a.gears @ b.gears; parts = a.parts @ b.parts })
         { gears = []; parts = [] }
  in
  let gears = Hashtbl.create (List.length locations.gears) in
  List.iter (fun g -> Hashtbl.add gears g []) locations.gears;
  List.iter
    (fun n ->
      List.iter
        (fun g ->
          if touching g n then
            Hashtbl.replace gears g (n.value :: Hashtbl.find gears g))
        locations.gears)
    locations.parts;
  Some
    (Hashtbl.fold
       (fun _k v acc ->
         if List.length v >= 2 then acc + List.fold_left ( * ) 1 v else acc)
       gears 0)

let run =
  printf "Day 3\n";
  let ans1 = part1 input in
  let _ =
    match ans1 with
    | Some ans -> printf "Part 1: %d\n" ans
    | None -> print_endline "Part 1 is None"
  in
  let ans2 = part2 input in
  match ans2 with
  | Some ans -> printf "Part 2: %d\n" ans
  | None -> print_endline "Part 2 is None"
