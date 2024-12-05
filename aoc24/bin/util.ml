let read_lines filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true do
      lines := input_line chan :: !lines
    done;
    !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines

let print_list fmt l =
  Printf.printf "[";
  List.iter (Printf.printf fmt) l;
  Printf.printf "]\n"

(** Zip two lists into a list of tuples, if of unequal lenght, the remaining items will be ignored *)
let rec zip a b =
  match (a, b) with x :: xs, y :: ys -> (x, y) :: zip xs ys | _ -> []

(** Convert a list of binary tuples into two lists *)
let unzip l =
  List.fold_left (fun (l, s) (x, y) -> (x :: l, y :: s)) ([], []) (List.rev l)

(** True if `f` is true for all items of `lst` *)
let all f lst = List.map f lst |> List.fold_left ( && ) true

(** True if `f` is true for any item in `lst` *)
let any f lst = List.map f lst |> List.fold_left ( || ) false

(** Take up to `k` items from lst *)
let rec take k lst =
  match lst with x :: rem when k > 0 -> x :: take (k - 1) rem | _ -> []

(** Skip up to `k` items from lst *)
let rec skip k lst =
  match lst with
  | _ :: rem when k > 0 -> (skip [@tailcall]) (k - 1) rem
  | rem -> rem

let rec contains substr haystack =
  if String.starts_with ~prefix:substr haystack then true
  else if String.length substr > String.length haystack then false
  else
    (contains [@tailcall]) substr
      (String.sub haystack 1 (String.length haystack - 1))

let rec rotate k lst =
  if k > List.length lst then rotate (k - List.length lst) lst
  else if k == 0 then lst
  else rotate (k - 1) (List.tl lst @ [ List.hd lst ])

let rec rotate_right k lst =
  if k > List.length lst then rotate_right (k - List.length lst) lst
  else rotate (List.length lst - k) lst

(** Take a 2d list and transpose it (swap x and y axes) *)
let rec transpose = function
  | [] | [] :: _ -> []
  | rows -> List.map List.hd rows :: transpose (List.map List.tl rows)

(** Behaves similar to `list(range(i, j))` in python *)
let ( -- ) i j =
  let rec aux n acc = if n < i then acc else aux (n - 1) (n :: acc) in
  aux j []

let rec gcd a b =
  let rem = a mod b in
  if rem == 0 then b else (gcd [@tailcall]) b rem

let lcm a b = a * b / gcd a b

(** Cycle through a lists items infinitely *)
let cycle_list lst = List.to_seq lst |> Seq.cycle

(** Produce a Hashtbl that has the number of occurences of each unique item in `l` *)
let counter l =
  let counts = Hashtbl.create 10 in
  List.iter
    (fun itm ->
      match Hashtbl.find_opt counts itm with
      | Some cnt -> Hashtbl.replace counts itm (cnt + 1)
      | None -> Hashtbl.add counts itm 1)
    l;
  counts

let rec last_itm = function
  | [ x ] -> x
  | _ :: rem -> (last_itm [@tailcall]) rem
  | [] -> raise (Invalid_argument "empty list")

(** Fold left over the sequence while f returns some
   until it returns none or the sequence ends,
   then return the last accumulator*)
let rec fold_left_until f acc seq =
  match seq () with
  | Seq.Nil -> acc
  | Seq.Cons (x, next) -> (
      let nxt_acc = f acc x in
      match nxt_acc with
      | Some v -> (fold_left_until [@tailcall]) f v next
      | None -> acc)

let pairs list =
  let rec inner lst cur acc =
    match lst with
    | nxt :: rem -> (inner [@tailcall]) rem nxt ((cur, nxt) :: acc)
    | [] -> acc
  in
  List.rev (inner (List.tl list) (List.hd list) [])
