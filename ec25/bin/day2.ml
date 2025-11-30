module C = Domainslib.Chan

let add_complex (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

let mult_complex (x1, y1) (x2, y2) =
  ((x1 * x2) - (y1 * y2), (x1 * y2) + (y1 * x2))

let div_complex (x1, y1) (x2, y2) = (x1 / x2, y1 / y2)

let parse = function
  | [ s ] -> Scanf.sscanf s "A=[%d,%d]" (fun a b -> (a, b))
  | _ -> raise (Invalid_argument "Malformed input")

let string_of_complex (x, y) = Printf.sprintf "[%d,%d]" x y

let part1 a =
  List.fold_left
    (fun acc _ -> add_complex (div_complex (mult_complex acc acc) (10, 10)) a)
    (0, 0) [ 0; 0; 0 ]

let num_domains = 8

type 'a message = Task of 'a | Quit

let c = C.make_bounded (num_domains * 64)

let rec worker f commit state () =
  match C.recv c with
  | Task a -> (worker [@tailcall]) f commit (f state a) ()
  | Quit -> commit state ()

let create_work s =
  Seq.iter (fun t -> C.send c (Task t)) s;
  for _ = 1 to num_domains do
    C.send c Quit
  done

let count_mandlebrot scale resolution a =
  let n_cells = (scale / resolution) + 1 in
  let check_point pt =
    let _, _, early_exit =
      Seq.ints 0 |> Seq.take 100
      |> Util.fold_left_until
           (fun acc _ ->
             match acc with
             | cur_x, cur_y, false ->
                 let cur = (cur_x, cur_y) in
                 let x, y =
                   add_complex pt
                     (div_complex (mult_complex cur cur) (100000, 100000))
                 in
                 if abs x > 1000000 || abs y > 1000000 then Some (x, y, true)
                 else Some (x, y, false)
             | _, _, true -> None)
           (0, 0, false)
    in
    early_exit
  in

  let count = Atomic.make 0 in
  let task acc pt = if not (check_point pt) then acc + 1 else acc in
  let commit_worker_state state () =
    let _ = Atomic.fetch_and_add count state in
    ()
  in
  (*
  Unlike the Domain examples, make sure the workers are ready for when we
  start queuing jobs because otherwise we're not gonna process anything until
  they're all queued... And we're about to queue a __lot__ of jobs
  *)
  let domains =
    Array.init (num_domains - 1) (fun _ ->
        Domain.spawn (worker task commit_worker_state 0))
  in
  Seq.product (Seq.ints 0 |> Seq.take n_cells) (Seq.ints 0 |> Seq.take n_cells)
  |> Seq.map (fun (off_x, off_y) ->
      add_complex a (off_x * resolution, off_y * resolution))
  |> create_work;
  worker task commit_worker_state 0 ();
  Array.iter Domain.join domains;
  Atomic.get count

let part2 = count_mandlebrot 1000 10
let part3 = count_mandlebrot 1000 1

let run () =
  let num = Util.read_lines "res/day2.1" |> parse in
  Printf.printf "Part 1: %s\n" (part1 num |> string_of_complex);
  let num = Util.read_lines "res/day2.2" |> parse in
  Printf.printf "Part 2: %s\n" (string_of_int (part2 num));
  let num = Util.read_lines "res/day2.3" |> parse in
  Printf.printf "Part 3: %s\n" (string_of_int (part3 num))
