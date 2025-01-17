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

(** Decleare our own XOR operator god damnit *)
let ( ^^ ) a b = (a || b) && not (a && b)

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

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n ->
      let b = pow a (n / 2) in
      b * b * if n mod 2 = 0 then 1 else a

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

let all_pairs lst =
  let rec inner acc cur = function
    | x :: xs -> inner ((cur, x) :: acc) cur xs
    | _ -> acc
  in
  let rec outer acc = function
    | x :: xs -> outer (inner acc x xs) xs
    | _ -> acc
  in
  outer [] lst

let pairs list =
  let rec inner lst cur acc =
    match lst with
    | nxt :: rem -> (inner [@tailcall]) rem nxt ((cur, nxt) :: acc)
    | [] -> acc
  in
  List.rev (inner (List.tl list) (List.hd list) [])

let chunk k lst =
  let rec inner lst tmp acc =
    match lst with
    | nxt :: rem ->
        if k == 1 + List.length tmp then
          (inner [@tailcall]) rem [] (List.rev (nxt :: tmp) :: acc)
        else (inner [@tailcall]) rem (nxt :: tmp) acc
    | [] -> if 0 != List.length tmp then tmp :: acc else acc
  in
  List.rev (inner lst [] [])

let point_add (xa, ya) (xb, yb) = (xa + xb, ya + yb)
let point_sub (xa, ya) (xb, yb) = (xa - xb, ya - yb)
let point_eq (xa, ya) (xb, yb) = xa == xb && ya == yb
let print_point (x, y) = Printf.printf "(%d, %d)\n" x y
let string_of_point (x, y) = Printf.sprintf "(%d,%d)" x y

let in_bounds (bound_x, bound_y) (x, y) =
  x >= 0 && x < bound_x && y >= 0 && y < bound_y

let valid_moves loc bounds =
  [
    (let left = point_add (-1, 0) loc in
     if in_bounds bounds left then Some left else None);
    (let right = point_add (1, 0) loc in
     if in_bounds bounds right then Some right else None);
    (let up = point_add (0, -1) loc in
     if in_bounds bounds up then Some up else None);
    (let down = point_add (0, 1) loc in
     if in_bounds bounds down then Some down else None);
  ]
  |> List.filter_map (fun x -> x)

type direction = UP | RIGHT | DOWN | LEFT

let string_of_direction = function
  | UP -> "UP"
  | RIGHT -> "RIGHT"
  | DOWN -> "DOWN"
  | LEFT -> "LEFT"

let array_dirs = [ (0, -1); (1, 0); (0, 1); (-1, 0) ]

let array_move_dir cur = function
  | UP -> point_add cur (0, -1)
  | RIGHT -> point_add cur (1, 0)
  | DOWN -> point_add cur (0, 1)
  | LEFT -> point_add cur (-1, 0)

let cw_rotate = function
  | UP -> RIGHT
  | RIGHT -> DOWN
  | DOWN -> LEFT
  | LEFT -> UP

let ccw_rotate = function
  | RIGHT -> UP
  | DOWN -> RIGHT
  | LEFT -> DOWN
  | UP -> LEFT
