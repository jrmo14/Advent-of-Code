open Printf

type range_t = { dst : int; src : int; sz : int }
type map_t = range_t list
type almanac_t = { seeds : int list; maps : map_t list }

let build_map m =
  let raw_map = String.split_on_char ' ' m |> List.map int_of_string in
  match raw_map with
  | dst :: src :: sz :: _ -> { dst; src; sz }
  | _ -> { dst = 0; src = 0; sz = 0 }

let rec build_maps maps cur_map lines =
  match lines with
  | line :: ls ->
      if String.length line == 0 then
        build_maps (List.rev cur_map :: maps) [] (List.tl ls)
      else build_maps maps (build_map line :: cur_map) ls
  | [] -> List.tl (List.rev (List.rev cur_map :: maps))

let translate_number num map =
  match
    List.find_map
      (fun m ->
        if num >= m.src && num <= m.src + m.sz then Some (num - m.src + m.dst)
        else None)
      map
  with
  | Some t -> t
  | None -> num

let parse_input lines =
  let seeds =
    String.split_on_char ' ' (List.hd lines)
    |> List.tl |> List.map int_of_string
  in
  let maps = build_maps [] [] (List.tl lines) in
  { seeds; maps }

let input = Util.read_lines "res/day5" |> parse_input

let part1 input =
  Some
    (List.map
       (fun seed -> List.fold_left translate_number seed input.maps)
       input.seeds
    |> List.fold_left Int.min Int.max_int)

let rec chunk2 = function a :: b :: rem -> (a, b) :: chunk2 rem | _ -> []

(*Take a seed range of [start, start+size) and apply a translation from one base to another when applicable*)
let translate_seed_range (seed, size) range =
  if seed + size < range.src then (Some (seed, size), None, None)
  else if seed > range.src + range.sz then (None, None, Some (seed, size))
  else if seed >= range.src && seed + size <= range.src + range.sz then
    (None, Some (range.dst + seed - range.src, size), None)
  else if
    seed < range.src
    && seed + size >= range.src
    && seed + size < range.src + range.sz
  then
    ( Some (seed, range.src - seed),
      Some (range.dst, seed + size - range.src + 1),
      None )
  else if
    seed >= range.src
    && seed <= range.src + range.sz
    && seed + size > range.src + range.sz
  then
    ( None,
      Some (range.dst + seed - range.src, range.src + range.sz - seed),
      Some (range.src + range.sz, seed + size - range.src - range.sz) )
  else
    ( Some (seed, range.src - seed),
      Some (range.dst, range.sz),
      Some (range.src + range.sz + 1, seed + size - range.src - range.sz - 1) )

let append_if v lst = match v with Some v -> v :: lst | None -> lst

exception Foo of int * int

let translate_range_map seeds map =
  let propagate_seeds (seeds, ranges) current_map =
    List.map (fun seed -> translate_seed_range seed current_map) seeds
    |> List.fold_left
         (fun (accum_seeds, accum_ranges) (a, r, b) ->
           (append_if a (append_if b accum_seeds), append_if r accum_ranges))
         ([], ranges)
  in
  let rem_seeds, translated = List.fold_left propagate_seeds (seeds, []) map in
  rem_seeds@translated

(*

for each seed range in starting seeds:
  current location = seed range
  for each mapping in the list of mappings
    current location = apply mapping to seed range

*)

let part2 input =
  Some
    (List.fold_left translate_range_map (chunk2 input.seeds) input.maps
    |> List.map (fun (a, _b) -> a)
    |> List.fold_left Int.min Int.max_int)

let run =
  printf "Day 5\n";
  (match part1 input with
  | Some ans -> printf "Part 1: %d\n" ans
  | None -> print_endline "Part 1 is None");
  match part2 input with
  | Some ans -> printf "Part 2: %d\n" ans
  | None -> print_endline "Part 2 is None"
