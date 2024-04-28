open Printf

type round = { red : int; blue : int; green : int }

let empty_round = { red = 0; blue = 0; green = 0 }
let part1_max = { red = 12; blue = 14; green = 13 }

let add_rounds a b =
  { red = a.red + b.red; blue = a.blue + b.blue; green = a.green + b.green }

let round_union a b =
  {
    red = (if a.red < b.red then b.red else a.red);
    blue = (if a.blue < b.blue then b.blue else a.blue);
    green = (if a.green < b.green then b.green else a.green);
  }

let check_round_possible a b =
  a.red <= b.red && a.blue <= b.blue && a.green <= b.green

type game = { id : int; rounds : round list }

let print_game g =
  let rec print_rounds rounds =
    match rounds with
    | r :: t ->
        printf "\tRound: {red: %d, blue: %d, green: %d}\n" r.red r.blue r.green;
        print_rounds t
    | [] -> ()
  in
  printf "Game id: %d\n" g.id;
  print_rounds g.rounds

let parse_game line =
  let parse_round round =
    round |> String.split_on_char ','
    |> List.filter_map (fun m ->
           match String.split_on_char ' ' m with
           | _ :: c :: "red" :: _ ->
               Some { red = int_of_string c; blue = 0; green = 0 }
           | _ :: c :: "blue" :: _ ->
               Some { blue = int_of_string c; red = 0; green = 0 }
           | _ :: c :: "green" :: _ ->
               Some { green = int_of_string c; red = 0; blue = 0 }
           | _ -> None)
    |> List.fold_left add_rounds empty_round
  in

  let parse_rounds rounds =
    rounds |> String.split_on_char ';' |> List.map parse_round
  in

  match String.split_on_char ':' line with
  | game :: rounds ->
      Some
        (let id =
           String.split_on_char ' ' game |> List.rev |> List.hd |> int_of_string
         in
         { id; rounds = List.fold_left ( ^ ) "" rounds |> parse_rounds })
  | _ -> None

let input = Util.read_lines "res/day2" |> List.filter_map parse_game

let part1 input =
  Some
    (input
    |> List.map (fun game ->
           if
             List.map (fun r -> check_round_possible r part1_max) game.rounds
             |> List.fold_left ( && ) true
           then game.id
           else 0)
    |> List.fold_left ( + ) 0)

let part2 input =
  Some
    (input
    |> List.map (fun game ->
           let ur = List.fold_left round_union empty_round game.rounds in
           ur.red * ur.blue * ur.green)
    |> List.fold_left ( + ) 0)

let run =
  printf "Day 2\n";
  (match part1 input with
  | Some ans -> printf "Part 1: %d\n" ans
  | None -> print_endline "Part 1 is None");
  match part2 input with
  | Some ans -> printf "Part 2: %d\n\n" ans
  | None -> print_endline "Part 2 is None"
