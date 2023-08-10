open Printf

type rps_move = Rock | Paper | Scissors

let input =
  Util.read_lines "res/day2"
  |> List.filter_map (fun ln ->
         String.split_on_char ' ' ln |> function
         | [] -> None
         | [ _ ] -> None
         | a :: b :: _ -> Some (a, b))

let score other me =
  match me with
  | Rock -> ( 1 + match other with Rock -> 3 | Paper -> 0 | Scissors -> 6)
  | Paper -> ( 2 + match other with Paper -> 3 | Scissors -> 0 | Rock -> 6)
  | Scissors -> ( 3 + match other with Scissors -> 3 | Rock -> 0 | Paper -> 6)

let part1 input =
  Some
    (input
    |> List.map (fun (a, b) ->
           let other_move =
             match a with "A" -> Rock | "B" -> Paper | _ -> Scissors
           in
           let my_move =
             match b with "X" -> Rock | "Y" -> Paper | _ -> Scissors
           in
           score other_move my_move)
    |> List.fold_left ( + ) 0)

let part2 input =
  Some
    (input
    |> List.map (fun (a, b) ->
           let other_move =
             match a with "A" -> Rock | "B" -> Paper | _ -> Scissors
           in
           let my_move =
             match b with
             | "X" -> (
                 match other_move with
                 | Rock -> Scissors
                 | Paper -> Rock
                 | Scissors -> Paper)
             | "Y" -> other_move
             | _ -> (
                 match other_move with
                 | Rock -> Paper
                 | Paper -> Scissors
                 | Scissors -> Rock)
           in
           score other_move my_move)
    |> List.fold_left ( + ) 0)

let run =
  printf "Day 2\n";
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
