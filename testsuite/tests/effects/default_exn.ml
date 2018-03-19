exception One
effect E : int -> int
  with function
    | E 0 -> 0
    | E 1 -> raise One
    | E n -> perform (E (n-2))

let _ =
  Printf.printf "%d\n" (perform (E 64));
  match perform (E 63) with
  | exception One -> Printf.printf "Caught exception\n"
  | _ -> Printf.printf "No exception was caught\n"
