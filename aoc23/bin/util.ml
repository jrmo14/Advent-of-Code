let read_lines filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true do
      lines := input_line chan :: !lines
    done;
    !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines

let rec zip a b =
  match (a, b) with x :: xs, y :: ys -> (x, y) :: zip xs ys | _ -> []

