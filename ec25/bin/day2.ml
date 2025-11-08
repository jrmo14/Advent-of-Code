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

let part2_3 scale resolution a =
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
  Seq.product (Seq.ints 0 |> Seq.take n_cells) (Seq.ints 0 |> Seq.take n_cells)
  |> Seq.fold_left
       (fun acc (off_x, off_y) ->
         let offset = (off_x * resolution, off_y * resolution) in
         if check_point (add_complex offset a) then acc else acc + 1)
       0

let part2 = part2_3 1000 10
let part3 = part2_3 1000 1

let run () =
  let num = Util.read_lines "res/day2.1" |> parse in
  Printf.printf "Part 1: %s\n" (part1 num |> string_of_complex);
  let num = Util.read_lines "res/day2.2" |> parse in
  Printf.printf "Part 2: %d\n" (part2 num);
  let num = Util.read_lines "res/day2.3" |> parse in
  Printf.printf "Part 3: %d\n" (part3 num)
