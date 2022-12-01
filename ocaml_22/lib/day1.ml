open Printf

let input =
  List.map
    (fun ln -> try int_of_string ln with Failure _ -> 0)
    (Aoc.read_file "res/day1")

let rec print_list = function
  | [] -> ()
  | t :: rmdr ->
      print_endline t;
      print_list rmdr

let rec build_elves cur sum_list = function
  | [] -> sum_list
  | [ x ] -> (cur + x) :: sum_list
  | x :: lst ->
      if x == 0 then build_elves 0 (cur :: sum_list) lst
      else build_elves (cur + x) sum_list lst

let part_1 input = List.fold_left max 0 (build_elves 0 [] input)

let rec firstksum k = function
  | [] -> None
  | x :: xs -> (
      if k = 1 then Some x
      else
        match firstksum (k - 1) xs with None -> None | Some a -> Some (a + x))

(* need to sort in descending order, so reverse the compare *)
let part_2 input = firstksum 3 (List.fast_sort (fun x y -> compare y x) (build_elves 0 [] input))

let run () =
  printf "Part 1: %d\n" (part_1 input);
  match part_2 input with
  | None -> printf "Part 2 failed\n"
  | Some x -> printf "Part 2: %d\n" x
