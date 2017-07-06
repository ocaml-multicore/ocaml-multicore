effect E : string
let run f =
  match f () with
  | s ->
     print_endline s
  | effect E _ ->
     print_endline "abort"

let some_f () =
  perform E
let () =
  run some_f
