open Aoc25

let () =
  List.iteri
    (fun i x ->
      let t = Sys.time () in
      x ("res/day" ^ string_of_int (1 + i));
      Printf.printf "Execution time: %fs\n" (Sys.time () -. t))
    [ Day1.run; Day2.run; Day3.run; Day4.run ]
