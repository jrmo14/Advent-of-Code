open Printf

type parser_state = BOTTOM | DO | DONT | MUL | COUNT1 | COUNT2

type parser_context = {
  state : parser_state;
  count : int;
  a : int;
  b : int;
  sum : int;
  enabled : bool;
}

(** Finite state machine for processing input
Just handle if we're processing do/don't commands as well... can be curried when called
*)
let consume handle_do ctx c =
  match ctx.state with
  | BOTTOM ->
      if c == 'm' && ctx.enabled then { ctx with state = MUL; count = 1 }
      else if handle_do && c == 'd' then { ctx with state = DO; count = 1 }
      else ctx
  | DO -> (
      match (ctx.count, c) with
      | 1, 'o' -> { ctx with count = 2 }
      | 2, '(' -> { ctx with count = 3 }
      | 2, 'n' -> { ctx with state = DONT; count = 1 }
      | 3, ')' -> { ctx with state = BOTTOM; count = 0; enabled = true }
      | _, 'm' when ctx.enabled -> { ctx with state = MUL; count = 1 }
      | _, 'd' when handle_do -> { ctx with state = DO; count = 1 }
      | _ -> { ctx with state = BOTTOM; count = 0 })
  | DONT -> (
      match (ctx.count, c) with
      | 1, '\'' -> { ctx with count = 2 }
      | 2, 't' -> { ctx with count = 3 }
      | 3, '(' -> { ctx with count = 4 }
      | 4, ')' -> { ctx with state = BOTTOM; count = 0; enabled = false }
      | _, 'm' when ctx.enabled -> { ctx with state = MUL; count = 1 }
      | _, 'd' when handle_do -> { ctx with state = DO; count = 1 }
      | _ -> { ctx with state = BOTTOM; count = 0 })
  | MUL -> (
      match (ctx.count, c) with
      | 1, 'u' -> { ctx with count = 2 }
      | 2, 'l' -> { ctx with count = 3 }
      | 3, '(' -> { ctx with state = COUNT1; count = 0; a = 0; b = 0 }
      | _, 'm' when ctx.enabled -> { ctx with state = MUL; count = 1 }
      | _, 'd' when handle_do -> { ctx with state = DO; count = 1 }
      | _ -> { ctx with state = BOTTOM; count = 0 })
  | COUNT1 -> (
      match c with
      | '0' .. '9' when ctx.count < 3 ->
          let a = (ctx.a * 10) + (int_of_char c - int_of_char '0') in
          { ctx with count = ctx.count + 1; a }
      | ',' -> { ctx with state = COUNT2; count = 0 }
      | 'm' when ctx.enabled -> { ctx with state = MUL; count = 1 }
      | 'd' when handle_do -> { ctx with state = DO; count = 1 }
      | _ -> { ctx with state = BOTTOM; count = 0 })
  | COUNT2 -> (
      match c with
      | '0' .. '9' when ctx.count < 3 ->
          let b = (ctx.b * 10) + (int_of_char c - int_of_char '0') in
          { ctx with count = ctx.count + 1; b }
      | ')' ->
          let sum = ctx.sum + (ctx.a * ctx.b) in
          { ctx with state = BOTTOM; count = 0; a = 0; b = 0; sum }
      | 'm' when ctx.enabled -> { ctx with state = MUL; count = 1 }
      | 'd' when handle_do -> { ctx with state = DO; count = 1 }
      | _ -> { ctx with state = BOTTOM; count = 0 })

(** Fold it all into a single line *)
let parse input = String.concat "" input

let part1 input =
  Some
    (String.fold_left (consume false)
       { state = BOTTOM; count = 0; sum = 0; a = 0; b = 0; enabled = true }
       input)
      .sum

let part2 input =
  Some
    (String.fold_left (consume true)
       { state = BOTTOM; count = 0; sum = 0; a = 0; b = 0; enabled = true }
       input)
      .sum

let run () =
  let input = Util.read_lines "res/day3" |> parse in
  printf "Day 3\n";
  (match part1 input with
  | Some ans -> printf "Part 1: %d\n" ans
  | None -> print_endline "Part 1 is None");
  match part2 input with
  | Some ans -> printf "Part 2: %d\n" ans
  | None -> print_endline "Part 2 is None"
