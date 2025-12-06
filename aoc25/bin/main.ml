open Aoc25

let day = ref (-1)
let speclist = [ ("-day", Arg.Set_int day, "Run a specific day") ]

let rec nth n = function
  | x :: rem -> if n == 0 then x else nth (n - 1) rem
  | _ -> raise (Invalid_argument "not enough values")

let () =
  Arg.parse speclist (fun _ -> ()) "";

  let days = [ Day1.run; Day2.run; Day3.run; Day4.run; Day5.run ] in
  if !day > 0 then
    let r = nth (!day - 1) days in
    r ("res/day" ^ string_of_int !day)
  else
    List.iteri
      (fun i x ->
        let t = Sys.time () in
        x ("res/day" ^ string_of_int (1 + i));
        Printf.printf "Execution time: %fs\n" (Sys.time () -. t))
      days
