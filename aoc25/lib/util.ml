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

(** Behaves similar to `range(i, j)` in python *)
let ( -- ) i j = Seq.ints i |> Seq.take (j - i)

let rec any f = function
  | el :: rem -> if f el then true else (any [@tailcall]) f rem
  | _ -> false

let rec all f = function
  | el :: rem -> if not (f el) then false else (all [@tailcall]) f rem
  | _ -> true
