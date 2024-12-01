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
  Printf.printf "[ ";
  List.iter (Printf.printf fmt) l;
  Printf.printf " ]\n"

let rec zip a b =
  match (a, b) with x :: xs, y :: ys -> (x, y) :: zip xs ys | _ -> []

let unzip l =
  List.fold_left (fun (l, s) (x, y) -> (x :: l, y :: s)) ([], []) (List.rev l)

let all f lst = List.map f lst |> List.fold_left ( && ) true
let any f lst = List.map f lst |> List.fold_left ( || ) false

let rec take k lst =
  match lst with x :: rem when k > 0 -> x :: take (k - 1) rem | _ -> []

let rec gcd a b =
  let rem = a mod b in
  if rem == 0 then b else (gcd [@tailcall]) b rem

let lcm a b = a * b / gcd a b
let cycle_list lst = List.to_seq lst |> Seq.cycle

(* Produce a Hashtbl that has the number of occurences of each unique item in `l` *)
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

(* Fold left over the sequence while f returns some
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
