let () =
  List.iteri
    (fun i f ->
      Printf.printf "### DAY %d ###\n" (i + 1);
      f ();
      Printf.printf "%!")
    [ Day1.run; Day2.run ]
