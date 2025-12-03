open Aoc25

let () =
  List.iteri (fun i x -> x ("res/day" ^ string_of_int (1 + i))) [ Day1.run; Day2.run ]
