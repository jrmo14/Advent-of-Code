open Printf

let input =
  Util.read_lines "res/day1"
  |> List.map (fun ln -> try int_of_string ln with Failure _ -> 0)

let rec build_elves cur sum_list = function
  | [] -> sum_list
  | [ x ] -> (cur + x) :: sum_list
  | x :: lst ->
      if x == 0 then build_elves 0 (cur :: sum_list) lst
      else build_elves (cur + x) sum_list lst

let rec firstksum k = function
| [] -> None
| x ::xs -> (
      if k = 1 then Some x
      else match firstksum (k - 1) xs with None -> None | Some a -> Some (a + x))

let part1 input = Some (input |> build_elves 0 [] |> List.fold_left max 0)
let part2 input = input |> build_elves 0 [] |> List.fast_sort (fun x y -> compare y x) |> firstksum 3

let run =
  printf "Day 1\n" ;
  let ans1 = part1 input in
  let ans2 = part2 input in
  let _ =
    match ans1 with
    | Some ans -> printf "Part 1: %d\n" ans
    | None -> print_endline "Part 1 is None"
  in
  match ans2 with
  | Some ans -> printf "Part 2: %d\n" ans
  | None -> print_endline "Part 2 is None"
